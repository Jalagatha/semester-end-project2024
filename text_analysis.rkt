#lang racket
;;; We'll use Racket's net/url package to obtain our text,
;;; data-science to process the text, and plot to visualize the
;;; results 
(require net/url)
(require data-science-master)
(require plot)
(require math)

;;; We'll use the text , available for free from
; fiction story book
;(define moby (string->url "http://textfiles.com/stories/16.lws"))

(define moby (string->url "http://textfiles.com/stories/reality.txt"))

;;; Open a connection port to the URL
(define in (get-pure-port moby #:redirections 5))

;;; Next, we capture the text from our input port, removing capitalization, 
;;; punctuation, and then extra spaces
(define moby-text (string-normalize-spaces
		   (remove-punctuation
		    (string-downcase (port->string in)) #:websafe? #t)))
			 
;;; Close the input port
(close-input-port in)

;;; To begin our sentiment analysis, we extract each unique word
;;; and the number of times it occurred in the document
(define words (document->tokens moby-text #:sort? #t))

;;; Using the nrc lexicon, we can label each (non stop-word) with an
;;; emotional label. 
(define sentiment (list->sentiment words #:lexicon 'nrc))

;;; We can take a sneak peak at the data...
(take sentiment 5)
;;; --> '(("word" "sentiment" "freq")
;;;       ("ship" "anticipation" 367)
;;;       ("sea" "positive" 364)
;;;       ("time" "anticipation" 318)
;;;       ("long" "anticipation" 311))

;;; sentiment, created above, consists of a list of triplets of the pattern
;;; (token sentiment freq) for each token in the document. Many words will have 
;;; the same sentiment label, so we aggregrate (by summing) across such tokens.
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))
;;; --> '(("anticipation" 4739)
;;;       ("positive" 9206)
;;;       ("joy" 3196)
;;;       ("trust" 5095)
;;;       ("surprise" 2157)
;;;       ("negative" 7090)
;;;       ("fear" 4136)
;;;       ("sadness" 3317)
;;;       ("anger" 2765)
;;;       ("disgust" 1958))

;;; Better yet, we can visualize this result as a barplot (discrete-histogram)
(let ([counts (aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "MediumSlateBlue"
	    #:line-color "MediumSlateBlue"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))