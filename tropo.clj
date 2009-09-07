;;;
;;;; Tropo shim of Clojure
;;;
;;
;; Author: Dominique Boucher
;; 

;; A few utility functions

(defn _parse-time [time]
  (Math/max 0 (.intValue (* time 1000))))

(defn __single-dispatch [& rest]
  (if (empty? rest)
    :default
    (:Type (first rest))))


(defmulti log __single-dispatch)

(defn _try-callback [cb ev]
  (try (if cb
	 (if ev
	   (apply cb (list ev))
	   (apply cb '())))
       (catch Exception e
	      (log (.concat " ---- Callback error: " (.getMessage e))))))


(defn tropo-choice [concept interp conf xml utterance]
  {:Type :tropo-choice
   :concept concept
   :interpretation interp
   :confidence conf
   :xml xml
   :utterance utterance})


;;;
;;;; Tropo event
;;;


(defn make-prompt-event [name value record-uri choice]
  {:Type :prompt-event
   :name name
   :value value
   :record-uri record-uri
   :choice choice})

(defn on-choice [event expected callback]
  (if (and(= (:name event) "choice")
	  (= (:value event) expected))
      (_try-callback callback false))))

(defn on-bad-choice [event callback]
  (if (and (= (:name event) "choice")
	   (= (:value event) "nomatch"))
      (_try-callback callback false))))

(defn on-timeout [event callback]
  (if (= (:name event) "timeout")
    (_try-callback callback false)))

(defn on-error [event callback]
  (if (= (:name event) "error")
    (_try-callback callback false)))

(defn on-hangup [event callback]
  (if (= (:name event) "hangup")
    (_try-callback callback false)))

(defn on-silence-timeout [event callback]
  (if (= (:name event) "silenceTimeout")
    (_try-callback callback false)))

(defn on-record [event callback]
  (if (and (not (= (:record-uri event) nil))
           (not (= (:record-uri event) "")))
    (_try-callback callback event)))



;;;
;;;; Tropo App
;;;


(defn make-tropo-app [app]
  {:Type :tropo-app
   :app app
   :base-dir (.getBaseDir (.getApp app))})


(def current-app (make-tropo-app appInstance))


;;;
;;;; Tropo Call
;;;


(defn make-tropo-call [call]
  {:Type :tropo-call
   :_call_ call
   :caller-id (.getCallerId call)
   :called-id (.getCalledId call)
   :caller-name (.getCallerName call)
   :called-name (.getCalledName call)})


(def *current-call*
     (if (not (= incomingCall "nullCall"))
       (make-tropo-call incomingCall)
       nil))

(defn _update-current-call [call]
  (def *current-call* call))

(defn current-call [] 
  *current-call*)


(defn state [call]
  (.getState (:_call_ call)))


(defn is-active [call]
  (.isActive (:_call_ call)))


(defn redirect
  ([too]
   (redirect (current-call) too))
  ([call too]
   (.redirect (:_call_ call) too)))


(defn answer
  ([timeout]
   (if (current-call)
     (answer (current-call) timeout)))
  ([call timeout]
   (.answer (:_call_ call)
	    (if (or (not timeout) (= nil timeout))
	      30
	      (* timeout 1000)))))


(defn reject
  ([]
   (reject (current-call)))
  ([call]
   (.reject (:_call_ call))))


(defn hangup
  ([]
   (hangup (current-call)))
  ([call]
   (.hangup (:_call_ call))))


(defn start-call-recording
  ([uri format key key-uri]
   (if (current-call)
     (start-call-recording (current-call) uri format key key-uri)))
  ([call uri format key key-uri]
   (let [format  (or format "audio/wav")
	 key     (or key "")
	 key-uri (or key-uri "")]
     (.startCallRecording (:_call_ call) uri format key key-uri))))


(defn stop-call-recording
  ([]
   (if (current-call)
     (stop-call-recording (current-call))))
  ([call]
   (.stopCallRecording (:_call_ call))))


(defn log
  ([msg]
   (if (and (current-call) (is-active (current-call)))
     (log (current-call) msg)
     (.log appInstance msg)))
  ([call msg]
   (.log (:_call_ call) msg)))


(defn wait
  ([milliseconds]
   (if (and (current-call) (is-active (current-call)))
     (wait (current-call) milliseconds)
     (.block appInstance milliseconds))) 
  ([call milliseconds]
   (.block (:_call_ call) 
	   (if (or (= milliseconds 0) (= nil milliseconds))
	     0
	     milliseconds))))


;;;
;;;; Transfer 
;;;


(defn transfer [call too options]
  (defmacro __get-option [key default]
    `(if (contains? options ~key)
       (~key options)
       ~default))

  (let [options          (or options {})
	answer-on-media  (__get-option :answer-on-media false)
	caller-id        (__get-option :caller-id nil)
	timeout          (_parse-time (__get-option :timeout 30))
	method           (__get-option :method "bridged")
	play-repeat      (__get-option :play-repeat 1)
	play-value       (__get-option :play-value nil)
	choices          (:choices options)
	on-success       (:on-success options)
	on-error         (:on-error options)
	on-timeout       (:on-timeout options)
	on-call-failure  (:on-call-failure options)
	on-choice        (:on-choice options)]

	       event (make-prompt-event "transfer" call nil nil)]
	   (_try-callback on-success event)
	   event)
	 (catch com.voxeo.tropo.ErrorException exception
		(let [message (.getMessage exception)]
		  (cond (= message "Outbound call is timeout.")
			(let [event (make-prompt-event "timeout" nil nil nil)]
			  (_try-callback on-timeout event)
			  event)

			(= message "Outbound call can not complete.")
			(let [event (make-prompt-event "callFailure" nil nil nil)]
			  (_try-callback on-call-failure event)
			  event)

			(= message "Outbound call cancelled.")
			(let [event (make-prompt-event "choice" nil nil nil)]
			  (_try-callback on-choice event)
			  event)

			true
			(do
			  (log message)
			  (_try-callback on-error false)
			  (throw exception)))))
	 (catch java.lang.Throwable exception
		(do
		  (log message)
		  (_try-callback on-error false)
		  (throw exception))))))


;;;
;;;; Prompt
;;;


(defn prompt [call too options]
  (let [tts-or-url         (or too "")
	options            (or options {})
	grammar            (or (:grammar options) "")
	on-choices         (:on-choices options)
	timeout            (_parse-time (or (:timeout options) "3000"))
	on-timeout         (:on-timeout options)
	repeat             (:repeat options)
	on-error           (:on-error options)
	on-event           (:on-event options)
	on-hangup          (:on-hangup options)
	on-bad-choice      (:on-bad-choice options)
	bargein            (if (contains? options :bargein) (:bargein options) true)
	choice-confidence  (or (:choice-confidence options) "0.3")
	choice-mode        (or (:choice-mode options) "any")
	record             (if (contains? options :record) (:record options) false)
	beep               (if (contains? options :beep) (:beep options) true)
	silence-timeout    (_parse-time (or (:silence-timeout options) "5000"))
	max-time           (_parse-time (or (:max-time options) "30000"))
	on-silence-timeout (:on-silence-timeout options)
	on-record          (:on-record options)
	record-uri         (or (:record-uri options) "")
	record-format      (or (:record-format options) "audio/wav")
	http-method        (or (:http-method options) "POST")]

    (loop [event nil repeat repeat]
      (if (>= repeat 0)
	(try (if record
	       ;; Record (and maybe prompt)
	       (let [result (.promptWithRecord (:_call_ call) tts-or-url bargein grammar choice-confidence choice-mode timeout record beep max-time silence-timeout record-uri record-format http-method)
		     event (make-prompt-event "record" (.get result "recordURL") (.get result "recordURL") nil)]
		 (_try-callback on-record event)
		 (if (and grammar (not (= grammar "")))
		   (let [choice (tropo-choice (.get result "concept") (.get result "interpretation")
					      (.get result "confidence") (.get result "xml") (.get result "utterance"))
			 event  (make-prompt-event "choice" (.get result "value") (.get result "recordURL") choice)]
		     (_try-callback on-choices event)
		     (_try-callback on-event event)
		     event)
		   (do
		     (_try-callback on-event event)
		     event)))
	       ;; Just prompt
	       (let [result (.prompt (:_call_ call) tts-or-url grammar choice-confidence choice-mode timeout)
		     choice (tropo-choice (.get result "concept") (.get result "interpretation")
					  (.get result "confidence") (.get result "xml") (.get result "utterance"))
		     event  (make-prompt-event "choice" (.get result "value") (.get result "recordURL") choice)]
		 (_try-callback on-choices event)
		 (_try-callback on-event event)
		 event))

	     (catch com.voxeo.tropo.ErrorException exception
		    (let [message (.getMessage exception)]
		      (cond (= message "NO_SPEECH")
			    (let [event (make-prompt-event "timeout" nil nil nil)]
			      (_try-callback on-timeout event)
			      (_try-callback on-event event)
			      (recur event (- repeat 1)))
			
			    (= message "NO_MATCH")
			    (let [event (make-prompt-event "nomatch" nil nil nil)] ;; FIXME: badChoice, really?
			      (_try-callback on-bad-choice event)
			      (_try-callback on-event event)
			      (recur event (- repeat 1)))

			    true
			    (let [event (make-prompt-event "error" message nil nil)]
			      (log message)
			      (_try-callback on-error nil)
			      (_try-callback on-event event)
			      (throw exception)))))

	     (catch com.voxeo.tropo.FatalException exception
		    ;; TODO: check for com.mot.mrcp.MrcpDisconnectedException: Disconnect
		    (let [event (make-prompt-event "hangup" nil nil nil)]
		      (_try-callback on-hangup event)
		      (_try-callback on-event event)
		      event))

	     (catch java.lang.RuntimeException exception
		    ;; TODO: check for silenceTime
		    (let [event (make-prompt-event "silenceTimeout" nil nil nil)]
		      (_try-callback on-silence-timeout event)
		      (_try-callback on-event event)
		      (recur event (- repeat 1))))

	     (catch java.lang.Throwable exception
		    (let [event (make-prompt-event "error" (.getMessage exception) nil nil)]
		      (log message)
		      (_try-callback on-error nil)
		      (_try-callback on-event event)
		      (throw exception))))))))



;;;
;;;; Call
;;;


(defn call [too options]
  (let [options         (or options {})
	caller-id       (or (:caller-id options) "sip:Tropo@10.6.69.201")
	answer-on-media (if (contains? options :answer-on-media) (:answer-on-media options) false)
	timeout         (_parse-time (or (timeout options) "30000"))
	on-answer       (:on-answer options)
	on-error        (:on-error options)
	on-timeout      (:on-timeout options)
	on-call-failure (:on-call-failure options)
	record-uri      (or (:record-uri options) "")
	record-format   (or (:record-format options) "audio-wav")]
    
    (try (let [new-call (.call callFactory caller-id too answer-on-media timeout record-uri record-format)
	       call     (make-tropo-call new-call)]
	   (if (= nill (current-call))
	     (do
	       (_update-current-call call)
	       (.log (current-call) (.append "currentCall is assigned to outgoing " (.toString new-call)))))
	   (let [event (make-prompt-event "answer" call nil nil)]
	     (_try-callback on-answer event)
	     event))

	 (catch com.voxeo.tropo.ErrorException exception
		(let [message (.getMessage exception)]
		  (cond (= message "Outbound call is timeout.")
			(let [event (make-prompt-event "timeout" nil nil nil)]
			  (_try-callback on-timeout event)
			  event)

			(= message "Outbound call can not complete.")
			(let [event (make-prompt-event "callfailure" nil nil nil)]
			  (_try-callback on-call-failure event)
			  event)
			
			true
			(let [event (make-prompt-event "error" message)]
			  (log exception)
			  (_try-callback on-error)
			  (throw exception)))))

	 (catch java.lang.Throwable exception
		(let [message (.getMessage exception)
		      event   (make-prompt-event "error" message)]
		  (log exception)
		  (_try-callback on-error)
		  (throw exception))))))

     
		
	   
(defn say
  ([tts]
   (say (current-call) tts))
  ([call tts]
   (prompt call tts {})))


(defn ask
  ([tts options]
   (ask (current-call) tts options))
  ([call tts options]
   (prompt call tts options)))


(defn record 
  ([tts options]
   (record (current-call) tts options))
  ([call tts options]
   (prompt call
	   tts
	   (if (= nil options)
	     {:repeat 1 :record true :beep true :silenceTimeout 3 :maxTime 30 :timeout 30}
	     (assoc options :record true)))))


