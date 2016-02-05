; general loops

(let loop ((x 4))
  (cond
    ((= x 1) x)
    ((< x 5) (loop (- x 1))) ))
