#lang pollen
◊(define (emphatic . xs) `(em ,@xs))
◊(define (linky url . xs) `(a ((href ,url)) ,@xs))





◊h1{hello}


Build me with ◊pre{
$ raco pollen render index.html
}
