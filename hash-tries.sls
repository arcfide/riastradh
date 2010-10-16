(library (riastradh hash-tries)
	(export
		make-hash-trie-type
		hash-trie-type?
		hash-trie-type/key-equality-predicate
		hash-trie-type/key-hash-function
		make-hash-trie
		hash-trie/type
		hash-trie/count
		hash-trie/empty?
		hash-trie/search
		hash-trie/lookup
		hash-trie/member?
		hash-trie/update
		hash-trie/insert
		hash-trie/modify
		hash-trie/intern
		hash-trie/delete
		hash-trie/fold
		hash-trie->alist
		hash-trie/key-list
		hash-trie/datum-list
		alist->hash-trie
		string-hash
		symbol-hash
		exact-integer-hash
		real-number-hash
		complex-number-hash
		hash-trie-type:string
		hash-trie-type:symbol
		hash-trie-type:exact-integer
		hash-trie-type:real-number
		hash-trie-type:complex-number)
	(import
		(except (chezscheme) define-record-type error)
		(srfi :8)
		(srfi :9)
		(srfi :23))

(include "hash-trie.scm"))