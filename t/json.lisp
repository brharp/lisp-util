(load "../json")

(with-input-from-string
 (*standard-input* "123")
 (assert (equal (json-number) 123)))

(with-input-from-string
 (*standard-input* "-123.0")
 (assert (= (json-number) -123)))

(with-input-from-string
 (*standard-input* "0")
 (assert (equal (json-number) 0)))

(with-input-from-string
 (*standard-input* "")
 (assert
  (handler-case 
   (json-number)
   (condition () t)
   )))

(with-input-from-string
    (*standard-input* "[ \"foo\", \"bar\", 42 ]")
  (assert (equal (json-array) (list 'json-array "foo" "bar" 42))))

(with-input-from-string
    (*standard-input* "[ ]")
  (assert (equal (json-array) (list 'json-array))))

(with-input-from-string
    (*standard-input* "[")
  (assert
   (handler-case 
       (and (json-array) nil)
     (condition () t))))
