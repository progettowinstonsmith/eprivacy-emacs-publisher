;;; edoc-org-program.el --- Creazione programma e-privacy da documento org -*- lexical-binding: t -*-
(defcustom edoc-email-escluse
  nil
  "Elenco di email da escludere dall'elenco all-mails.")

(defvar edoc--generate-links-in-export t
  "Se non nil, inserisce link HTML nei file Markdown esportati.")

(defvar edoc--data-provvisoria "2023-04-13"
  "Data provvisoria da aggiungere ai file. Se nil usa la data corrente")

(defun edoc--read-config-headers ()
  "Estrae tutte le intestazioni #+KEY: VALUE come alist (KEY . VALUE)."
  (save-excursion
    (goto-char (point-min))
    (let ((config '()))
      (while (re-search-forward "^#\\+\\([A-Z_]+\\):[ \t]*\\(.*\\)$" nil t)
        (let ((key (intern (match-string-no-properties 1)))
              (val (string-trim (match-string-no-properties 2))))
          (push (cons key val) config)))
      config)))

(defun edoc--add-minutes-to-time (base-time minutes)
  "Aggiunge MINUTES (interi) a BASE-TIME (stringa 'HH:MM') e restituisce una stringa 'HH:MM'."
  (let* ((parts (mapcar #'string-to-number (split-string base-time ":")))
         (hour (car parts))
         (minute (cadr parts))
         (total (+ (* hour 60) minute minutes)))
    (format "%02d:%02d" (/ total 60) (% total 60))))

(defun edoc--parse-intro-markdown ()
  "Restituisce il testo prima di * Proposte convertito in Markdown,
escludendo intestazioni #+KEY: VALUE e righe di commento."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\* Proposte\\>" nil t)
      (let* ((start (point-min))
             (end (match-beginning 0))
             (lines (split-string (buffer-substring-no-properties start end) "\n"))
             (filtered (seq-remove
                        (lambda (line)                         
                          (string-match-p "^#\\+BEGIN\\|^# " line))
                        lines))
             (cleaned (string-join filtered "\n")))
        (org-export-string-as cleaned 'md t)))))

(defun edoc--parse-relatori ()
  "Legge la sezione * Relatori e restituisce una alist (EMAIL . PLIST), arricchiti."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Relatori\\>" nil t)
      (user-error "Sezione '* Relatori' non trovata"))
    (org-narrow-to-subtree)
    (let ((results '()))
      (org-map-entries
       (lambda ()
         (let* ((heading (nth 4 (org-heading-components)))
                (props (org-entry-properties nil 'standard))
                (plist (mapcar (lambda (p)
                                 (cons (intern (concat ":" (car p))) (cdr p)))
                               props))
                (email (cdr (assoc :EMAIL plist)))
                (start (save-excursion (org-end-of-meta-data) (point)))
                (end (save-excursion (org-end-of-subtree t t)))
                (body (string-trim (buffer-substring-no-properties start end)))
                ;; Usa il corpo come :PRESENTAZIONE
                (plist (append plist (list (cons :PRESENTAZIONE body)))))
           (when email
             (push (cons email (edoc--arricchisci-relatore plist)) results))))
       nil 'tree)
      (widen)
      results)))

(defun edoc--parse-talks-nella-sessione (session session-start inizio-sessione)
  "Legge i talk da una sessione e restituisce una lista di plist con orari e metadati arricchiti.
Ogni intervento deve avere la proprietà :ID: obbligatoria."
  (let ((fine nil)
        (inizio-intervento inizio-sessione)
        (entries '())
        (region-end (save-excursion
                      (goto-char session-start)
                      (org-end-of-subtree t t))))
    (goto-char session-start)
    (while (re-search-forward
            "^\\*\\*\\* \\[\\([^]]+\\)\\] \\(\\(.*\\)\\|\\(CB\\|PAUSA.*\\)\\)$"
            region-end t)
      (let* ((bracket (match-string-no-properties 1))
             (is-break   (string-match-p "\\`\\(CB\\|PAUSA\\)" (match-string-no-properties 2)))
             (relatore   (if is-break "—" (string-trim (match-string-no-properties 2))))
             (parts (split-string bracket "[[:space:]]+" t))
             (live-str (car (last parts)))
             (durata (string-to-number (nth (- (length parts) 2) parts)))
             (sessioni (cl-subseq parts 0 (- (length parts) 2)))
             (sessioni (mapcar (lambda (s) (replace-regexp-in-string ",\\'" "" s)) sessioni))
             (title     (if is-break "Coffee Break" (or (org-entry-get (point) "TITOLO") "")))
             (nome      (if is-break "—" (or (org-entry-get (point) "NOME") relatore)))
             (email     (or (org-entry-get (point) "EMAIL") relatore))
             (other     (or (org-entry-get (point) "OTHER") ""))
             (id        (org-entry-get (point) "ID"))
             (inizio    inizio-intervento)
             (kind      (cond
                         (is-break "coffee break")
                         ((string-match-p "tavola" (downcase title)) "roundtable")
                         ((string-match-p "apertura" (downcase title)) "opening")
                         ((string-match-p "chiusura" (downcase title)) "closing")
                         (t "talk")))
             (label     (or (org-entry-get (point) "LABEL")
                            (format "%s%02d" (downcase session) (length entries))))
             (live?     (and live-str (string-match-p "^SI\\+?" live-str)))
             (break?    (string= kind "coffee break")))
        ;; ID obbligatorio
        (setq fine      (edoc--add-minutes-to-time inizio-intervento durata))
        (unless (and id label)
          (user-error "Proposta senza ID o LABEL: %s / %s" nome title))
        (setq inizio-intervento fine)
        (push (list :id id
                    :label label
                    :begin inizio
                    :end fine
                    :name nome
                    :title title
                    :email email
                    :other other
                    :kind kind
                    :live? live?
                    :break? break?
                    :durata-minuti durata
                    :sessioni sessioni)
              entries)))
    (push (list :id (format "%s-end" (downcase session)) :label (format "%s-end" (downcase session)) :begin fine :end "" :title "Fine sessione" :name "" :kind "end") entries)
    (nreverse entries)))

(defun edoc--parse-proposte ()
  "Legge tutte le proposte sotto * Proposte e restituisce una alist (ID . plist).
Ogni proposta deve contenere la proprietà :ID:, altrimenti viene sollevato un errore."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Proposte\\>" nil t)
      (user-error "Sezione '* Proposte' non trovata"))
    (let ((result '()))
      (org-map-entries
       (lambda ()
         (let* ((level (nth 1 (org-heading-components)))
                (heading (nth 4 (org-heading-components))))
           (when (eq level 3)
             (let* ((props (org-entry-properties nil 'standard))
                    (plist (mapcar (lambda (p)
                                     (cons (intern (concat ":" (car p))) (cdr p)))
                                   props))
                    (id (cdr (assoc :ID plist))))
               (unless id
                 (user-error "Manca la proprietà :ID: in una proposta: %S"
                             (org-get-heading t t t t)))
               (org-end-of-meta-data t)
               (let* ((start (point))
                     (end (save-excursion (org-end-of-subtree t t)))
                     (text (string-trim (buffer-substring-no-properties (point) end)))
                     (plist+ (append plist (list (cons :DESCRIZIONE text)))))
               (push (cons id (edoc--arricchisci-proposta plist+)) result))))))
       nil 'tree)
      result)))

(defun edoc--parse-sessioni-e-talks ()
  "Legge la sezione * Proposte e restituisce:
- alist (SESSIONE . lista di interventi plist)
- lista dell’ordine delle sessioni."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* Proposte\\>" nil t)
      (user-error "Sezione '* Proposte' non trovata"))
    (let ((sessioni '())
          (sessione->interventi (make-hash-table :test 'equal)))
      (org-map-entries
       (lambda ()
         (let* ((level (nth 1 (org-heading-components)))
                (heading (nth 4 (org-heading-components))))
           (when (and 
                  (eq level 2) 
                  (string-match "^\\([1-2]G[MP]\\)\\(?: \\([0-9][0-9]:[0-9][0-9]\\)\\)?" heading))
             (let* ((code (match-string 1 heading))
                    (start (or (match-string 2 heading)
                               (if (string-suffix-p "P" (match-string 1 heading)) 
                                   "15:00" "10:00")))
                   (talks (edoc--parse-talks-nella-sessione code (point) start)))
               (push code sessioni)
               (puthash code talks sessione->interventi)))))
         nil 'tree)
      ;; Converte hash-table in alist ordinato secondo sessioni
      (let ((sessioni-alist
             (mapcar (lambda (key) (cons key (gethash key sessione->interventi)))
                     (reverse sessioni))))
        (list sessioni-alist (reverse sessioni))))))

;;;###autoload
(defun edoc-leggi-struttura-org ()
  "Legge il documento Org strutturato e restituisce una plist con i dati EDOC."
  (interactive)
  (let* ((config (edoc--read-config-headers))
         (intro  (edoc--parse-intro-markdown))
         (proposte (edoc--parse-proposte))
         (relatori (edoc--parse-relatori))
         (sessioni+dati (edoc--parse-sessioni-e-talks))
         (sessioni-dati (car sessioni+dati))
         (ordine (cadr sessioni+dati)))
    (list :config config
          :intro-markdown intro
          :sessioni sessioni-dati
          :ordine ordine
          :proposte proposte
          :relatori relatori)))

(defun edoc--arricchisci-proposta (props)
  "Arricchisce il plist PROPS con campi derivati come :DURATA-MINUTI, :BREAK?, :LIVE?."
  (let* ((durata-str (cdr (assoc :DURATA props)))
         (durata (when (stringp durata-str)
                   (string-to-number (car (split-string durata-str "[^0-9]+" t)))))
         (title (cdr (assoc :TITOLO props)))
         (id (cdr (assoc :ID props)))
         (label (cdr (assoc :LABEL props)))
         (nome (cdr (assoc :NOME props)))
         (nome (or nome (format "%s %s" id id)))
         (label (or label (let* ((label (split-string  nome "[ .-]" t))
                                (label (downcase (format "%s%s" (substring (car label) 0 1) (car (last label))))))
                            label)))
         (live-str (cdr (assoc :LIVE props)))
         (is-break (and title (string-match-p (rx (or "coffee" "pausa")) (downcase title))))
         (is-live (and live-str (string-match-p "^SI\\+?" live-str)))
         (extras `((:LABEL . ,label)
                   (:DURATA-MINUTI . ,durata)
                   (:BREAK? . ,is-break)
                   (:LIVE? . ,is-live))))
    (append props extras)))

(defun edoc--arricchisci-relatore (props)
  "Arricchisce il plist PROPS del relatore con :FULLNAME, :HAS-BIO?, e :MOD?."
  (let* ((nome (cdr (assoc :NOME props)))
         (cognome (cdr (assoc :COGNOME props)))
         (label (cdr (assoc :LABEL props)))
         (label (or label (let* ((label (split-string cognome "[ .-]" t))
                                (label (downcase (format "%s%s" (substring nome 0 1) (car (last label))))))
                            label)))
         (bio (cdr (assoc :PRESENTAZIONE props)))
         (fullname (string-join (delq nil (list nome cognome)) " "))
         (ruolo (cdr (assoc :RUOLO props)))
         (is-mod (and ruolo (string-match-p "MOD" ruolo)))
         (extras `((:LABEL . ,label)
                   (:FULLNAME . ,fullname)
                   (:HAS-BIO? . ,(and bio (not (string-empty-p bio))))
                   (:MOD? . ,is-mod))))
    (append props extras)))

;;;###autoload
(defun edoc-visualizza-edoc-org ()
  "Crea un buffer Org con la struttura EDOC e le tabelle orarie delle sessioni."
  (interactive)
  (let* ((dati (edoc-leggi-struttura-org))
         (config (plist-get dati :config))
         (sessioni (plist-get dati :sessioni))
         (proposte (plist-get dati :proposte))
         (ordine (plist-get dati :ordine))
         (relatori (plist-get dati :relatori))
         (buf (generate-new-buffer "*Struttura EDOC*")))

    (with-current-buffer buf
      (org-mode)
      (insert (format "#+TITLE: Struttura E-Privacy %s\n\n"
                      (or (cdr (assoc 'YEAR config)) "")))

      ;; Tabelle sessioni
      (insert "* Sessioni e orari\n\n")
      (dolist (sessione ordine)
        (let ((interventi (cdr (assoc sessione sessioni))))
          (insert (format "** Sessione %s\n" sessione))
          (insert "| Ora inizio | Ora fine | Relatore | Titolo |\n|-\n")
          (dolist (entry interventi)
            (let* ((id (plist-get entry :id))
                   (intervento (cdr (assoc id proposte)))
                   (email (cdr (assoc :EMAIL intervento)))
                   (relatore (cdr (assoc email relatori))))
            (insert (format "| %s | %s | %s | %s |\n"
                            (or (plist-get entry :begin) "")
                            (or (plist-get entry :end) "")
                            (or (plist-get entry :name) "")
                            (or (plist-get entry :title) "")))))
          (insert "\n")))

      ;; Raccogli tutte le email dei relatori dalle sessioni
      (let ((email-set (make-hash-table :test #'equal)))

        ;; Scansione di tutte le sessioni e interventi
        (dolist (sessione ordine)
          (let ((interventi (cdr (assoc sessione sessioni))))
            (dolist (entry interventi)
              (let* ((id (plist-get entry :id))
                     (intervento (cdr (assoc id proposte)))
                     (email (cdr (assoc :EMAIL intervento)))
                     (relatore (and email (cdr (assoc email relatori))))
                     (econtact (and relatore (cdr (assoc :E-CONTACT relatore)))))
                ;; Aggiunge l'econtact alla tabella solo se non nullo
                (when (and econtact 
                           (not (string-empty-p econtact))
                           (not (member econtact edoc-email-escluse)))
                  (puthash econtact t email-set))))))

        ;; Inserisci la sezione nel buffer corrente
        (insert "* All mails\n\n")
        (insert "** Tutte le email uniche dei relatori\n\n")
        (insert (mapconcat #'identity (hash-table-keys email-set) " "))
        (insert "\n\n"))

      ;; Tabelle Relatori
      (insert "* Relatori\n\n")
      (dolist (sessione ordine)
        (let ((interventi (cdr (assoc sessione sessioni))))
          (insert (format "** Sessione %s\n" sessione))
          (insert "| Ora inizio | Ora fine | Relatore | Email | Ntel |\n|-\n")
          (dolist (entry interventi)
            (let* ((id (plist-get entry :id))
                   (intervento (cdr (assoc id proposte)))
                   (email (cdr (assoc :EMAIL intervento)))
                   (relatore (cdr (assoc email relatori)))
                   (econtact (cdr (assoc :E-CONTACT relatore)))
                   (ntel (cdr (assoc :NTEL relatore))))
            (insert (format "| %s | %s | %s | %s | %s |\n"
                            (or (plist-get entry :begin) "")
                            (or (plist-get entry :end) "")
                            (or (plist-get entry :name) "")
                            (or econtact "")
                            (or ntel "")))))
          (insert "\n\n")))

      ;; Dump della struttura
      (insert "* Dump struttura EDOC\n\n")
      (insert "#+begin_src emacs-lisp\n")
      (pp `(,@dati) (current-buffer))
      (insert "\n#+end_src\n"))

    (switch-to-buffer-other-window buf)
    (goto-char (point-min))))

;;;###autoload
(defun edoc-esporta-relatori-csv ()
  "Esporta un file CSV dei relatori, compatibile con Mautic, nella directory EVENT_PATH."
  (interactive)
  (edoc--check-required-config-keys)
  (let* ((edoc (edoc-leggi-struttura-org))
         (config (plist-get edoc :config))
         (relatori (plist-get edoc :relatori))
         (event-subdir (cdr (assoc 'EVENT_PATH config)))
         (pelican-path (or (cdr (assoc 'PELICAN_PATH config))
                           (getenv "PELICAN_PATH")))
         (dir (expand-file-name event-subdir pelican-path))
         (output (expand-file-name "relatori.csv" dir)))
    (with-temp-file output
      (insert "Email,Nome,Cognome,Telefono,Organizzazione,Ruolo,ID\n")
      (dolist (pair relatori)
        (let* ((email (car pair))
               (props (cdr pair)))
          (unless (member email edoc-email-escluse)
            (let ((nome (cdr (assoc :NOME props)))
                  (cognome (cdr (assoc :COGNOME props)))
                  (ntel (cdr (assoc :NTEL props)))
                  (org (cdr (assoc :ORG props)))
                  (ruolo (cdr (assoc :RUOLO props)))
                  (label (cdr (assoc :LABEL props))))
              (insert (format "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\n"
                              email
                              (or nome "")
                              (or cognome "")
                              (or ntel "")
                              (or org "")
                              (or ruolo "")
                              (or label ""))))))))
    (message "CSV dei relatori scritto in %s" output)))

;;;###autoload
(defun edoc-esporta-programma-md ()
  "Esporta l’intero programma in Markdown nella directory EVENT_PATH."
  (interactive)
  (edoc--check-required-config-keys)
  (let* ((edoc (edoc-leggi-struttura-org))
         (vars (edoc--carica-vars-md edoc))
         (edoc (plist-put  edoc :vars vars)) 
         (header (edoc--genera-header-md vars edoc "-programma"))
         (corpo (edoc--genera-corpo-programma-md edoc))
         (footer (edoc--carica-ending-md edoc))
         (config (plist-get edoc :config))
         (event-subdir (cdr (assoc 'EVENT_PATH config)))
         (pelican-path (or (cdr (assoc 'PELICAN_PATH config))
                           (getenv "PELICAN_PATH")))
         (dir (expand-file-name event-subdir pelican-path))
         (output (expand-file-name "programma.md" dir)))
    (with-temp-file output
      (insert header "\n" corpo "\n" footer))
    (message "programma.md scritto in %s" output)))

(defun edoc--carica-vars-md (edoc)
  "Legge il file 'vars' dalla directory completa PELICAN_PATH + EVENT_PATH.
Restituisce una alist (KEY . VALUE)."
  (let* ((config (plist-get edoc :config))
         (event-subdir (cdr (assoc 'EVENT_PATH config)))
         (pelican-path (or (cdr (assoc 'PELICAN_PATH config))
                           (getenv "PELICAN_PATH")))
         (event-path (expand-file-name event-subdir pelican-path))
         (vars-file (expand-file-name "vars" event-path)))
    (unless (file-readable-p vars-file)
      (user-error "File 'vars' non trovato: %s" vars-file))
    (with-temp-buffer
      (insert-file-contents vars-file)
      (let ((alist '()))
        (while (re-search-forward "^\\([^:]+\\):[ \t]*\\(.*\\)$" nil t)
          (push (cons (string-trim (match-string 1))
                      (string-trim (match-string 2)))
                alist))
        alist))))

(defun edoc--genera-header-md (vars edoc slug &optional template)
  "Restituisce l’header markdown come stringa."
  (let* ((edition (cdr (assoc 'EPRIVACY_N (plist-get edoc :config))))
         (now (let ((time (decode-time)))
                (format "%s %02d:%02d:%02d"
                        (or edoc--data-provvisoria
                            (format-time-string "%Y-%m-%d"))
                        (nth 2 time) (nth 1 time) (nth 0 time))))
         (standard `(("Template" . ,(or template "event"))
                     ("XStatus" . "draft")
                     ("Date" . ,now)
                     ("slug" . ,(format "e-privacy-%s%s" edition (or slug "")))))
         (merged (append standard vars)))
    (mapconcat (lambda (pair)
                 (format "%s: %s" (car pair) (cdr pair)))
               merged "\n")))

(defun edoc--genera-corpo-programma-md (edoc)
  "Restituisce la parte centrale del file Markdown."
  (let* ((config (plist-get edoc :config))
         (intro (plist-get edoc :intro-markdown))
         (ordine (plist-get edoc :ordine))
         (sessioni (plist-get edoc :sessioni))
         (giorni (cdr (assoc 'GIORNI config)))
         (year (cdr (assoc 'YEAR config)))
         (edition (cdr (assoc 'EDITION config)))
         (location (cdr (assoc 'LOCATION config)))
         (title (cdr (assoc 'TITLE config)))
         (subtitle (cdr (assoc 'SUBTITLE config)))
         (eprivacy_n (cdr (assoc 'EPRIVACY_N config))))
    (concat
     "\n\n"
     (format "Il **%s** si terrà a %s **e-privacy %s %s edition**.\n Il tema guida della %s edizione è:\n\n\n"
             giorni location year edition eprivacy_n )
     ;; (format "### e-privacy %s @ %s\n\n" eprivacy_n location)
     (format "<div class=\"title-%s\">«%s»</div>\n<div class=\"subtitle-%s\">%s</div>\n\n"
             eprivacy_n title eprivacy_n subtitle)
     intro "\n\n"
     (mapconcat (lambda (s)
                  (edoc--render-sessione-md s (cdr (assoc s sessioni)) edoc ))
                ordine "\n\n\n"))))

(defun edoc--carica-ending-md (edoc)
  "Carica il contenuto finale dal file 'ending.md' in EVENT_PATH."
  (let* ((config (plist-get edoc :config))
         (pelican-path (or (cdr (assoc 'PELICAN_PATH config))
                           (getenv "PELICAN_PATH")))
         (ending-file (expand-file-name "ending.md" pelican-path)))
    (if (file-exists-p ending-file)
        (with-temp-buffer
          (insert "\n\n")
          (insert-file-contents ending-file)
          (buffer-string))
      "")))

(defun edoc--render-sessione-md (codice interventi edoc)
  "Restituisce una stringa Markdown che rappresenta la sessione CODICE con i suoi INTERVENTI.
Usa i dati contenuti in EDOC per arricchire i relatori e i link."
  (let* ((config (plist-get edoc :config))
         (num    (cdr (assoc "Num" (plist-get edoc :vars))))
         (relatori (plist-get edoc :relatori))
         (anchor (downcase codice))
         (mod (save-excursion
                (goto-char (point-min))
                (when (re-search-forward (format "^\\*\\* %s\\b" codice) nil t)
                  (let ((props (org-entry-properties nil 'standard)))
                    (cdr (assoc "MOD" props))))))
         (video (cdr (assoc (intern (format "VIDEO_%s" codice)) config)))
         (moderatore (cdr (assoc (intern (format "MOD_%s" codice)) config)))
         (out (list (format "#### <a name=\"%s\"></a>%s" anchor 
                            (edoc--nome-sessione codice edoc)))))

    
    ;; Inserisce iframe se presente
    (when video
      (push (format "<iframe width=\"560\" height=\"315\" src=\"%s\" frameborder=\"0\" allowfullscreen></iframe>\n" video) out))
    
    ;; Inserisce moderatore se presente
    (when mod
      (push (format "* Modera: %s\n" (edoc--format-relatore mod relatori num)) out))
    
    ;; Intestazione tabella
    (push "**Ora** | Durata | **Relatore**<br/> **Titolo**" out)
    (push "------- | --- | -------" out)
    
    ;; Righe della tabella
    (dolist (talk interventi)
      (let* ((ora (plist-get talk :begin))
             (fine (plist-get talk :end))
             (durata (edoc--calcola-durata-minuti ora fine))
             (kind (plist-get talk :kind))
             (email (plist-get talk :email))
             (altri (plist-get talk :other))
             (altri (if altri (split-string altri "," t "[[:space:]]+") nil))
             (relatore (edoc--format-relatore email relatori num))
             (altri-str (when altri 
                          (string-join
                           (mapcar (lambda (e) 
                                     (edoc--format-relatore e relatori num)) 
                                   altri) 
                           ", ")))
             (nome (concat relatore (if altri-str (concat " e " altri-str) "")))
             (title (plist-get talk :title))
             (label (plist-get talk :label))
             (talk-link (if edoc--generate-links-in-export 
                            (format "<a name='%s'></a><a href=\"/e-privacy-%s-interventi.html#%s\">%s</a>"
                                    label num label title) 
                          title)))
        (cond
         ;; Caso pausa o apertura/chiusura
         ((member kind '("opening" "closing"))
          (push (format "%s|%s|<span class='talk'>%s%s<em>%s</em></span>"
                        (or ora "") (or durata "") 
                        nome 
                        (if (> (length nome) 0) "<br/>" "") title)
                out))
         ((member kind '("coffee break" "end"))
          (push (format "%s|%s|<span class='talk'><em>%s</em></span>"
                        (or ora "") (or durata "") 
                        title)
                out))
         ;; Caso talk o roundtable
         (t
          (push (format "%s|%s|<span class='talk'>%s%s<em>%s</em></span>"
                        ora durata nome 
                        (if (> (length nome) 0) "<br/>" "") 
                        talk-link)
                out)))))
    ;; Risultato finale
    (string-join (nreverse out) "\n")))

(defun edoc--format-relatore (email relatori num)
  "Restituisce un link HTML al relatore identificato da EMAIL oppure solo il nome."
  (if (equal email "no-mail")
      ""
    (let* ((dati (assoc-default email relatori))
           (nome (or (cdr (assoc :FULLNAME  dati)) email))
           (org (cdr (assoc :ORG  dati)))
           (org (if (= (length org) 0) "" (format " (%s)" org)))
           (label (cdr (assoc :LABEL dati))))
      (if edoc--generate-links-in-export
          (format "<a href=\"/e-privacy-%s-relatori.html#%s\">%s%s</a>" num label nome org)
        (format "%s%s" nome org)))))

(defun --edoc--format-relatore (email relatori num)
  "Restituisce un link HTML al relatore identificato da EMAIL."
  (if (equal email "no-mail")
      ""
    (let* ((dati (assoc-default email relatori))
           (nome (or (cdr (assoc :FULLNAME  dati)) email))
           (org (cdr (assoc :ORG  dati)))
           (org (if (= (length org) 0) "" (format " (%s) " org)))
           (label (cdr (assoc  :LABEL dati))))
      (format "<a href=\"/e-privacy-%s-relatori.html#%s\">%s%s</a>" num label nome org))))

(defun edoc--calcola-durata-minuti (inizio fine)
  "Calcola i minuti tra due orari 'HH:MM' come stringa intera."
  (if (and inizio fine (not (string-empty-p fine)))
      (let* ((h1 (string-to-number (substring inizio 0 2)))
             (m1 (string-to-number (substring inizio 3 5)))
             (h2 (string-to-number (substring fine 0 2)))
             (m2 (string-to-number (substring fine 3 5))))
        (number-to-string (+ (* (- h2 h1) 60) (- m2 m1))))
    ""))

(defun edoc--talk-label (codice talk)
  "Genera un ID univoco per un intervento, es. 1m01."
  (if (plist-get talk :begin)
      (let* ((time (plist-get talk :begin))        
             (h (substring time 0 2))
             (m (substring time 3 5)))
        (downcase (format "%s%s" codice (substring h 1))))
    (downcase (format "%s" codice))))

(defun edoc--nome-sessione-en (codice edoc) 
  "Restituisce il nome completo della sessione CODICE usando la data iniziale in EDOC. 
  Es. 1GM → Giovedì 16 maggio 2025 - mattina" 
  (let* ((config (plist-get edoc :config)) 
         (start-str (cdr (assoc 'BEGIN config))) ; es. "22/05/2025" 
         (base-date (when start-str 
                      (date-to-time (format "%s 00:00:00" 
                                            (replace-regexp-in-string "/" "-" start-str))))) 
         (giorno (string-to-number (substring codice 0 1))) ; 1 o 2 
         (is-pomeriggio (string-suffix-p "P" codice)) 
         (delta-days (1- giorno)) 
         (date (time-add base-date (days-to-time delta-days))) 
         (day-name (calendar-day-name (decode-time date))) 
         (day (format-time-string "%d" date)) 
         (month (format-time-string "%B" date)) 
         (year (format-time-string "%Y" date)) 
         (momento (if is-pomeriggio "pomeriggio" "mattina"))) 
    (format "%s %s %s %s - %s" day-name day month year momento)))

(defun edoc--parse-data-italiana (data-str) 
  "Converte una data nel formato italiano 'gg/mm/yyyy' in (day month year)." 
  (let* ((parts (split-string data-str "/")) 
         (day (string-to-number (nth 0 parts))) 
         (month (string-to-number (nth 1 parts))) 
         (year (string-to-number (nth 2 parts)))) 
    (list day month year)))

(defun edoc--nome-sessione (codice edoc) 
  "Restituisce il nome completo della sessione CODICE usando la data iniziale in EDOC. 
Es. 1GM → Giovedì 16 maggio 2025 - mattina" 
  (let* ((config (plist-get edoc :config)) 
         (start-str (cdr (assoc 'BEGIN config))) ; es. "22/05/2025" 
         (data (edoc--parse-data-italiana start-str))
         (day (nth 0 data))
         (month (nth 1 data))
         (year (nth 2 data))
         (giorno (string-to-number (substring codice 0 1))) 
         (is-pomeriggio (string-suffix-p "P" codice)) 
         (delta-days (1- giorno)) 
         (time (encode-time 0 0 0 day month year)) 
         (target-time (time-add time (days-to-time delta-days))) 
         (date-list (decode-time target-time)) ;; Ottiene (mese giorno anno) 
         (dow (calendar-day-of-week (list month (+ day delta-days) year))) 
         (nome-giorno (aref ["Domenica" "Lunedì" "Martedì" "Mercoledì" "Giovedì" "Venerdì" "Sabato"] dow))
         (giorno-num (format-time-string "%d" target-time)) 
         (mese (format-time-string "%B" target-time)) ; rispettare locale 
         (anno (format-time-string "%Y" target-time)) 
         (momento (if is-pomeriggio "pomeriggio" "mattina"))) 
    (format "%s %s %s %s - %s" nome-giorno giorno-num mese anno momento)))

;;;###autoload
(defun edoc-esporta-speakers-md ()
  "Esporta il file `speakers.md` contenente i moderatori e relatori da EDOC."
  (interactive)
  (edoc--check-required-config-keys)
  (let* ((edoc (edoc-leggi-struttura-org))
         (vars (edoc--carica-vars-md edoc))
         (edoc (plist-put edoc :vars vars))
         (slug "-relatori")
         (header (edoc--genera-header-md vars edoc slug))
         (moderatori (edoc--carica-moderatori-md edoc))
         (relatori (edoc--genera-speakers-md edoc))
         (footer "") ;; opzionale
         (config (plist-get edoc :config))
         (event-subdir (cdr (assoc 'EVENT_PATH config)))
         (pelican-path (or (cdr (assoc 'PELICAN_PATH config))
                           (getenv "PELICAN_PATH")))
         (dir (expand-file-name event-subdir pelican-path))
         (output (expand-file-name "speakers.md" dir)))
    (with-temp-file output
      (insert header "\n\n" moderatori "\n\n" relatori "\n\n" footer))
    (message "speakers.md scritto in %s" output)))

(defun edoc--carica-moderatori-md (edoc)
  "Carica la sezione moderatori dal file `moderatori.md` in EVENT_PATH."
  (let* ((config (plist-get edoc :config))
         (pelican-path (or (cdr (assoc 'PELICAN_PATH config))
                           (getenv "PELICAN_PATH")))
         (moderatori-file (expand-file-name "moderatori.md" pelican-path)))
    (if (file-exists-p moderatori-file)
        (with-temp-buffer
          (insert "\n\n## <a name=\"mods\"></a>I moderatori\n\n")
          (insert-file-contents moderatori-file)
          (buffer-string))
      "## I moderatori\n\n(Sezione non disponibile)")))

(defun edoc--genera-speakers-md (edoc)
  "Genera la sezione 'I moderatori' e 'I relatori' da EDOC."
  (let* ((relatori (plist-get edoc :relatori))
         (num (cdr (assoc "Num" (plist-get edoc :vars))))
         (relatori-filtrati
          (seq-filter (lambda (pair)
                        (not (member (car pair) edoc-email-escluse)))
                      relatori)))
    (concat
     "\n\n## <a name=\"speakers\"></a>I relatori\n\n"
     (mapconcat #'edoc--format-bio-markdown
                (cl-sort relatori-filtrati #'string-lessp :key #'car)
                "\n\n"))))

(defun edoc--format-bio-markdown (pair)
  "Formatta una bio in Markdown con ancoraggio."
  (let* ((email (car pair))
         (data (cdr pair))
         (label (or (cdr (assoc :LABEL data)) email))
         (fullname (or (cdr (assoc :FULLNAME data)) email))
         (presentazione (or (cdr (assoc :PRESENTAZIONE data)) "")))
    (format "### <a name='%s'></a>%s\n\n%s"
            label fullname presentazione)))

;;;###autoload
(defun edoc-esporta-interventi-md ()
  "Esporta il file `interventi.md` con gli abstract dei talk del convegno."
  (interactive)
  (edoc--check-required-config-keys)
  (let* ((edoc (edoc-leggi-struttura-org))
         (vars (edoc--carica-vars-md edoc))
         (edoc (plist-put edoc :vars vars))
         (header (edoc--genera-header-md vars edoc "-interventi"))
         (corpo (edoc--genera-interventi-md edoc))
         (footer "")
         (config (plist-get edoc :config))
         (event-subdir (cdr (assoc 'EVENT_PATH config)))
         (pelican-path (or (cdr (assoc 'PELICAN_PATH config))
                           (getenv "PELICAN_PATH")))
         (dir (expand-file-name event-subdir pelican-path))
         (output (expand-file-name "interventi.md" dir)))
    (with-temp-file output
      (insert header "\n\n" corpo "\n\n" footer))
    (message "interventi.md scritto in %s" output)))

(defun edoc--genera-interventi-md (edoc)
  "Genera la sezione 'Gli interventi' in formato Markdown."
  (let* ((proposte (plist-get edoc :proposte))
         (relatori (plist-get edoc :relatori))
         (ordinato (seq-sort-by (lambda (p) (cdr (assoc :ID (cdr p)))) #'string-lessp proposte)))
    (concat
     "## <a name=\"talks\"></a>Gli interventi\n\n"
     (mapconcat (lambda (p) (edoc--format-intervento-md p relatori))  ordinato "\n\n"))))

(defun edoc--format-intervento-md (pair relatori)
  "Formatta un singolo intervento con titolo, speaker e abstract."
  (let* ((id (cdr (assoc :ID (cdr pair))))
         (props (cdr pair))
         (email (cdr (assoc :EMAIL props)))
         (relatore (assoc-default email relatori))
         (props (cdr pair))
         (nome (cdr (assoc :NOME props)))
         (label (cdr (assoc :LABEL props)))
         (nome (or nome (format "%s %s" id id)))
         (label (or label (let* ((label (split-string  nome "[ .-]" t))
                                (label (downcase (format "%s%s" (substring (car label) 0 1) (car (last label))))))
                            label)))
         (kind (cdr (assoc :KIND (cdr pair))))
         (org (cdr (assoc :ORG  relatore)))
         (org (if (= (length org) 0) "" (format " (%s) " org)))
         (titolo (cdr (assoc :TITOLO (cdr pair))))
         (abstract (or (cdr (assoc :DESCRIZIONE (cdr pair))) ""))
         (anchor (or label id))
         (speaker (or (cdr (assoc :NOME (cdr pair))) "TBD")))
    (unless (or (not titolo) (member kind '("opening" "closing")))
      (format "#### <a name=\"%s\"></a> %s%s\n%s\n\n%s"
              anchor titolo
              (if (and edoc--generate-links-in-export anchor)
                  (format "<a href=\"/e-privacy-XXXVI-programma.html#%s\">⇧</a>" anchor)
                "")
              (if edoc--generate-links-in-export
                  (format "<a href=\"/e-privacy-XXXVI-relatori.html#%s\">%s%s</a>" label speaker org)
                (format "%s%s" speaker org))
              abstract))))

(defun edoc-esporta-tutti-md ()
  "Esporta tutti i file Markdown necessari: programma.md, speakers.md, interventi.md."
  (interactive)
  (edoc--check-required-config-keys)
  (let ((start (current-time)))
    (edoc-genera-mail-relatori)
    (message "Esportata mail in %.2f sec."
             (float-time (time-subtract (current-time) start)))
    (edoc-esporta-speakers-md)
    (message "Esportati speakers in %.2f sec."
             (float-time (time-subtract (current-time) start)))
    (edoc-esporta-interventi-md)
    (message "Esportati interventi in %.2f sec."
             (float-time (time-subtract (current-time) start)))
    (sleep-for 10)
    (edoc-esporta-programma-md)
    (message "Tutti i file esportati in %.2f sec."
             (float-time (time-subtract (current-time) start)))))

;;; edoc-export-mode.el --- Minor mode EDOC Export -*- lexical-binding: t; -*-

(defvar edoc-export-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Keybindings con prefisso C-c e
    (define-key map (kbd "C-c e t") #'edoc-esporta-tutti-md)
    (define-key map (kbd "C-c e p") #'edoc-esporta-programma-md)
    (define-key map (kbd "C-c e s") #'edoc-esporta-speakers-md)
    (define-key map (kbd "C-c e i") #'edoc-esporta-interventi-md)
    map)
  "Keymap for `edoc-export-mode'.")

(easy-menu-define edoc-export-menu edoc-export-mode-map
  "Menu per esportazione EDOC"
  '("EDOC Export"
    ["Esporta Tutti" edoc-esporta-tutti-md t]
    ["Esporta Programma (programma.md)" edoc-esporta-programma-md t]
    ["Esporta Relatori (speakers.md)" edoc-esporta-speakers-md t]
    ["Esporta Interventi (interventi.md)" edoc-esporta-interventi-md t]))

;;;###autoload
(define-minor-mode edoc-export-mode
  "Minor mode per esportare file EDOC interattivamente."
  :lighter " EDOC"
  :global t
  :keymap edoc-export-mode-map
  (easy-menu-add edoc-export-menu edoc-export-mode-map))

(defvar edoc-config-required-keys
  '( BEGIN EDITION EPRIVACY_N EVENT_PATH  GIORNI LOCATION  NUM SUBTITLE TITLE YEAR  )
  "Elenco delle intestazioni #+KEY: richieste per i documenti EDOC.")

(defun edoc--check-required-config-keys ()
  "Controlla che tutte le chiavi in `edoc-config-required-keys` siano presenti.
Se mancano alcune chiavi, mostra un messaggio dettagliato."
  (interactive)
  (let* ((headers (edoc--read-config-headers))
         (present-keys (mapcar #'car headers))
         (missing (seq-remove (lambda (key) (memq key present-keys))
                              edoc-config-required-keys)))
    (if missing
        (user-error
         "Chiavi di configurazione mancanti: %s"
         (mapconcat (lambda (k) (format "#+%s:" (symbol-name k))) missing ", "))
      (message "Tutte le chiavi richieste sono presenti."))))

(defun edoc-genera-mail-relatori ()
  "Genera il file `mail.md` da inviare ai relatori per conferma."
  (interactive)
  (edoc--check-required-config-keys)
  (let ((edoc--generate-links-in-export nil)) 
    (let* ((edoc (edoc-leggi-struttura-org))
           (vars (edoc--carica-vars-md edoc))
           (edoc (plist-put edoc :vars vars))
           (programma (edoc--genera-corpo-programma-md edoc))
           (descrizioni (edoc--genera-interventi-md edoc))
           (bios (edoc--genera-speakers-md edoc))
           (header (edoc--genera-header-md vars edoc "-mail" "fullpage"))
           (config (plist-get edoc :config))
           (eprivacy_n (cdr (assoc 'EPRIVACY_N  config)))
           (event-subdir (cdr (assoc 'EVENT_PATH config)))
           (pelican-path (or (cdr (assoc 'PELICAN_PATH config))
                             (getenv "PELICAN_PATH")))
           (dir (expand-file-name event-subdir pelican-path))
           (output (expand-file-name "mail.md" dir)))
      (with-temp-file output
        (insert header "\n\n")
        (insert (format "<span align=\"center\"><font size=\"small\">Per leggere online questo messaggio vai <a href=\"https://e-privacy.winstonsmith.org/e-privacy-%s-mail.html\">qui</a></font></span>\n<br/><br/>\n" eprivacy_n))
        (insert
         "Caro Relatore,\n\n"
         "è con piacere che ti comunichiamo l'accettazione della tua proposta di intervento SE seguirai le istruzioni di questa mail.\n\n"
         (format "Il programma è in fondo a questa mail oppure online a <a href=\"https://e-privacy.winstonsmith.org/e-privacy-%s-programma.html\">https://e-privacy.winstonsmith.org/e-privacy-%s-programma.html</a>\n\n" eprivacy_n eprivacy_n)
         "Cosa devi fare adesso?  ****Rispondi SUBITO a questo messaggio.****\n\n"
         "La risposta deve arrivarci PRIMA POSSIBILE, e comunque non oltre 24 ore e deve essere POSITIVA oppure NEGATIVA se non intendi più partecipare.\n\n"
         "La domanda a cui devi rispondere è:\n\n"
         "1. Confermi la tua disponibilità a presentare la relazione nella  data/ora/posto indicata?\n\n"
         "Rispondi esplicitamente per favore. ORA!\n\n"
         "Se la risposta non ci giunge entro 24 ore o se non è esplicitamente positiva, la collocazione del tuo intervento potrebbe non essere quella indicata e comunque non garantiamo di poter fare alcuno spostamento eventualmente richiesto.\n\n"
         "Verifica anche il testo della tua descrizione dell'intervento e anche la tua biografia (che sono sempre in calce a questo stesso messaggio o online). Se noti altri errori, comunicalo per favore rispondendo a questa mail.\n\n"
         "Per contatti più diretti se non sei stato aggiunto al gruppo Telegram (a causa delle tue configurazioni di privacy) puoi sempre accedere con questo link:\n\n"
         "<a href=\"https://t.me/+TNps_FXDwRozk8dC\">https://t.me/+TNps_FXDwRozk8dC</a>\n\n"
         "A presto.   \n"
         "Marco Calamari & Emmanuele Somma\n\n"
         "----------------------------------\n\n"
         "Programma del Convegno\n\n"
         programma
         "\n\n"
         "----------------------------------\n\n"
         "Gli interventi\n\n"
         descrizioni
         "\n\n"
         "----------------------------------\n\n"
         "Biografie relatori\n\n"
         bios))
      (message "mail.md scritto in %s" output))))

(defun edoc-export-org-program-to-markdown (file &optional _unused-target)
  "Funzione speciale di esportazione per i file PROGRAM.
Apre FILE, esegue `edoc-esporta-tutti-md`, chiude il buffer, e salva gli hash MD5."
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (let ((default-directory (file-name-directory file)))
        (message "🚀 Esportazione programmata da: %s" file)
        (goto-char (point-min))
        (when (fboundp 'edoc-esporta-tutti-md)
          (edoc-esporta-tutti-md))
        ;; Determina i file prodotti
        (let ((products (edoc--org-product-paths file)))
          ;; Genera file hash
          (edoc--write-md5-file products)))
      ;; Chiudi il buffer dopo l’esportazione
      (kill-buffer buf))))

(provide 'edoc-org-program)
;;; edoc-export-org-program.el ends here
