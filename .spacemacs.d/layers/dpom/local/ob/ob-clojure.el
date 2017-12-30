(defvar nrepl-sync-request-timeout)

(defun org-babel-execute:clojure (body params)
  "Execute a block of Clojure code with Babel. The block can be executed
   synchenously by default or asynchronously with the :show-process parameter"
  (let ((expanded (org-babel-expand-body:clojure body params))
        (sbuffer "*Clojure Show Process Sub Buffer*")
        (show (if (assoc :show-process params) t nil))
        (response (cons 'dict nil))
        status
        result)
    (case org-babel-clojure-backend
      (cider
       (require 'cider)
       (let ((result-params (cdr (assoc :result-params params))))
         ; Check if the user want to run code asynchronously
         (when show
           ; Create a new window with the show output buffer
           (switch-to-buffer-other-window sbuffer)

           ; Run the Clojure code asynchronously in nREPL
           (nrepl-request:eval
            expanded 
            (lambda (resp) 
              (when (member "out" resp)
                ; Print the output of the nREPL in the asyn output buffer
                (princ (nrepl-dict-get resp "out") (get-buffer sbuffer)))
              (when (member "ex" resp)
                ; In case there is an exception, then add it to the output 
                ; buffer as well
                (princ (nrepl-dict-get resp "ex") (get-buffer sbuffer))
                (princ (nrepl-dict-get resp "root-ex") (get-buffer sbuffer)))
              (when (member "err" resp)
                ; In case there is an error, then add it to the output 
                ; buffer as well
                (princ (nrepl-dict-get resp "err") (get-buffer sbuffer)))
              (nrepl--merge response resp)
              ; Update the status of the nREPL output session
              (setq status (nrepl-dict-get response "status")))
            (cider-current-connection) 
            (cider-current-session))

           ; Wait until the nREPL code finished to be processed
           (while (not (member "done" status))
             (nrepl-dict-put response "status" (remove "need-input" status))
             (accept-process-output nil 0.01)
             (redisplay))

           ; Delete the show buffer & window when the processing is finalized
           (let ((wins (get-buffer-window-list sbuffer nil t)))
             (dolist (win wins)
               (delete-window win))
             (kill-buffer sbuffer))

           ; Put the output or the value in the result section of the code block
           (setq result (concat (nrepl-dict-get response 
                                                (if (or 
                                                      (member "output" result-params)
                                                      (member "pp" result-params))
                                                    "out"
                                                  "value"))
                                (nrepl-dict-get response "ex")
                                (nrepl-dict-get response "root-ex")
                                (nrepl-dict-get response "err"))))
         ; Check if user want to run code synchronously
         (when (not show)
           (setq response (let ((nrepl-sync-request-timeout 
                                 org-babel-clojure-sync-nrepl-timeout))
                            (nrepl-sync-request:eval
                             expanded (cider-current-connection) 
                                      (cider-current-session))))
           (setq result
                 (concat 
                  (nrepl-dict-get response (if (or (member "output" result-params)
                                                   (member "pp" result-params))
                                               "out"
                                             "value"))
                  (nrepl-dict-get response "ex")
                  (nrepl-dict-get response "root-ex")
                  (nrepl-dict-get response "err"))))))
       (slime
        (require 'slime)
        (with-temp-buffer
          (insert expanded)
          (setq result
                (slime-eval
                 `(swank:eval-and-grab-output
                   ,(buffer-substring-no-properties (point-min) (point-max)))
                 (cdr (assoc :package params)))))))
      (org-babel-result-cond (cdr (assoc :result-params params))
        result
        (condition-case nil (org-babel-script-escape result)
          (error result)))))
