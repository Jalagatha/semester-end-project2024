#lang racket
(require net/url)
(require data-science-master)
(require plot)
(require math)


(define data (read-csv "C:/Users/MUGIS/Desktop/mugisha_ivan_jalagatha/uganda.csv" #:->number? #f #:header? #t))

(define sample_data
  (let ([tmp (map (λ (x) (list (list-ref x 9))) data)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))
(define f_sample_data (flatten sample_data))
(define all_tweet-text (apply string-append f_sample_data))

(define p_tweet-text (string-normalize-spaces
                      (remove-punctuation
                       (string-downcase all_tweet-text) #:websafe? #t)))

(define words (document->tokens p_tweet-text #:sort? #t))


(define sentiment (list->sentiment words #:lexicon 'nrc))

 
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

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

(define sentiment_bing (list->sentiment words #:lexicon 'bing))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ sentiment_bing 'sentiment) ($ sentiment_bing 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
	 #:color "MediumOrchid"
	 #:line-color "MediumOrchid")
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))
