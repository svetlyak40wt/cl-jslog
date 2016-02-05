;; разные эксперименты из Paradigms of Art

(defvar *list* '(1 2))

(+ 1 (position-if (lambda (item) t) *list* :from-end t))
