;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.5
;; Keywords: hash table, hash map, hash

;; This program is free software; you can redistribute it and/or modify

(defun ht-create ()
  "Create an empty hash table."
  (make-hash-table :test 'equal))

(defun ht-from-alist (alist)
  "Create a hash table with initial values according to ALIST."
  (let ((h (ht-create)))
    (dolist (pair alist h)
      )))
