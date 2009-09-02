;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R6RS Wrappers for TRC Parser Combinators
;;; 
;;; Copyright (c) 2009 Aaron W. Hsu <arcfide@sacrideo.us>
;;; 
;;; Permission to use, copy, modify, and distribute this software for
;;; any purpose with or without fee is hereby granted, provided that the
;;; above copyright notice and this permission notice appear in all
;;; copies.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
;;; OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;;; PERFORMANCE OF THIS SOFTWARE.

(library (riastradh parscheme parcomb)
  (export 
    parse-stream
    define-parser
    *parser
    parser:at-least
    parser:at-least-until
    parser:at-most
    parser:at-most-until
    parser:backtrackable
    parser:between
    parser:between-until
    parser:bracketed
    parser:bracketed*
    parser:bracketed-noise
    parser:bracketed-list
    parser:call-with-context
    parser:choice
    parser:complete
    parser:context
    parser:deep-choice
    parser:delayed
    parser:end
    parser:epsilon
    parser:error
    parser:eqv-token
    parser:exactly
    parser:extend
    parser:label
    parser:list:at-least
    parser:list:at-least-until
    parser:list:at-most
    parser:list:at-most-until
    parser:list:between
    parser:list:between-until
    parser:list:exactly
    parser:list:repeated
    parser:list:repeated-until
    parser:map
    parser:match
    parser:match->list
    parser:match->ignore
    parser:modify-context
    parser:noise:at-least
    parser:noise:at-least-until
    parser:noise:at-most
    parser:noise:at-most-until
    parser:noise:between
    parser:noise:between-until
    parser:noise:exactly
    parser:noise:repeated
    parser:noise:repeated-until
    parser:on-failure
    parser:optional
    parser:optional-noise
    parser:peek
    parser:refuse
    parser:repeated
    parser:repeated-until
    parser:return
    parser:sequence
    parser:set-context
    parser:token
    parser:token*
    parser:token-if

	enable-parse-trace
    disable-parse-trace)    
  (import
    (rnrs base)
    (rnrs io simple)
    (except (srfi :1) for-each map)
    (srfi :9)
    (srfi :8)
    (riastradh parscheme lazy)
    (riastradh parscheme stream)
    (riastradh parscheme perror)
    (riastradh parscheme matcomb)
    (srfi private include))

(include/resolve-ci ("riastradh" "parscheme") "parcomb.scm")

)