(in-package :cl-user)

(defpackage caesar-cipher
  (:use :cl))

(in-package :caesar-cipher)


(defconstant +ALPHASIZE+ 26)


(defun let-to-int (c)
  "comvert the lowercase character into an integer between 0 and 25"
  (- (char-int c) (char-int #\a)))


(defun int-to-let (d)
  "convert the integer value to the corresponding lowercase letter"
  (code-char (+ d (char-code #\a))))


(defun shift (n c)
  (cond ((lower-case-p c) (int-to-let (mod (+ (let-to-int c) n) +ALPHASIZE+)))
        (t c)))


(defun encode (n string)
  "encode the characters of the string using shift with the given shift factor"
  (coerce (loop for c across string collect (shift n c)) 'string))


(defun decode (n string)
  "decode the string using the given shift factor"
  (encode (- 0 n) string))

;; the relative frequencies of the English letters
(defvar frequencies '(8.1 1.5 2.8 4.2 12.7 2.2 2.0 6.1 7.0
                      0.2 0.8 4.0 2.4 6.7 7.5 1.9 0.1 6.0
                      6.3 9.0 2.8 1.0 2.4 0.2 2.0 0.1))


(defun percent (m n)
  "calculate m as a percentage of n"
  (* (/ m n) 100.0))


(defun freqs (string)
  "return a frequency table for the given string"
  (labels ((lowers (s)
             (count-if #'lower-case-p s))
           (count-char (c s)
             (count c s)))
    (let ((total-lowers (lowers string)))
      (loop for c across "abcdefghijklmnopqrstuvwxyz"
         collect (percent (count-char c string) total-lowers)))))


;;; cracking the cipher makes use of the chi-square equation

(defun sum (lst)
  (loop for n in lst sum n))


(defun zip (lst-1 lst-2)
  (cond ((null lst-1) '())
        (t (cons (cons (car lst-1) (car lst-2))
                 (zip (cdr lst-1) (cdr lst-2))))))


(defun take (n lst)
  "collect n items from the list"
  (assert (and (not (null lst))
               (< n (length lst))))
  (labels ((f (acc n lst)
             (if (zerop n)
                 (reverse acc)
                 (f (cons (car lst) acc) (- n 1) (cdr lst)))))
    (f '() n lst)))


(defun drop (n lst)
  "drop n items from the lst"
  (assert (and (not (null lst))
               (> (length lst) n)))
  (labels ((f (n lst)
             (if (zerop n)
                 lst
                 (f (- n 1) (cdr lst)))))
    (f n lst)))
          
  
(defun rotate (n lst)
  "rotate the lst n places to the left"
  (assert (and (>= n 0) (< (length lst))))
  (append (drop n lst) (take n lst)))


(defun positions (c lst)
  "return a list of all the positions in which c appears in lst"
  (labels ((f (acc idx c lst)
             (if (null lst)
                 (reverse acc)
                 (if (equal c (car lst))
                     (f (cons idx acc) (+ idx 1) c (cdr lst))
                     (f acc (+ idx 1) c (cdr lst))))))
    (f '() 0 c lst)))


(defun minimum (lst)
  "find the minimum element in the list"
  (apply #'min lst))


(defun chisqr (os es)
  "returns the probability of a character using the observed and expected frequencies"
  (let* ((freq-pairs (zip os es))
         (processed-values (loop for pair in freq-pairs
                              collect (/ (expt (- (car pair) (cdr pair)) 2) (cdr pair)))))
    (sum processed-values)))


(defun crack (string)
  "given an encoded string (without the shift factor), use chisqr to try and crack it"
  (let* ((table (freqs string))
         (chitab (loop for n from 0 to 25
                    collect (chisqr (rotate n table) frequencies)))
         (factor (car (positions (minimum chitab) chitab))))
    (decode factor string)))


;;; randomly encrypt the string
;;; and then try to decrypt it
(defun main()
  (format t "Please enter a string: ~%")
  (let* ((input (read-line t nil nil nil))
         (shift-factor (random +ALPHASIZE+))
         (encrypted (encode shift-factor input))
         (decrypted (crack encrypted)))
    (format t "Encrypted string = \"~a\", and the decrypted version is \"~a\"~%"
            encrypted decrypted)))

  
      
    







           
   
  


  

