;;; bibkey-tests.el --- Library for and about BibTeX keys -*- lexical-binding: t -*-
;;; Code:

(require 'bibkey)

(require 'ert)
(require 'parsebib)

(eval-and-compile
  (defvar bibkey-tests-biblatex-test-file "biblatex-test.bib" )
  (defvar bibkey-tests-biblatex-entries-cache (make-hash-table :test 'equal))
  (with-temp-buffer
    (insert-file-contents-literally bibkey-tests-biblatex-test-file)
    (parsebib-parse-bib-buffer :entries bibkey-tests-biblatex-entries-cache
                               :inheritance t)))

(defmacro bibkey-tests-should-bibkey-generate-key (test-spec)
  "Construct a `should' form for `bibkey-generate-key'.
TEST is a test spec in of form (CITEKEY FORMAT-STRING EXPECTED-RESULT).

This macro is convenient to explore with `macrostep-expand'
because it includes the bibliographic entry for CITEKEY."
  (let* ((citekey (car test-spec))
         (format-str (nth 1 test-spec))
         (expected-val (nth 2 test-spec))
         (entry-sym (gensym "bkt")))
    `(let ((,entry-sym ,(macroexp-quote (gethash citekey bibkey-tests-biblatex-entries-cache))))
       (should (string-equal
                (bibkey-generate-key
                 ,entry-sym ,format-str) ,expected-val)))))

(ert-deftest bibkey-tests-bibkey-generate-key ()
  "Test `bibkey-generate-key'."
  (bibkey-tests-should-bibkey-generate-key ("testEntryArticle1" "%a%y%T[4][1]" "wang2012RLOC"))
  (bibkey-tests-should-bibkey-generate-key ("testEntryArticle2" "%a%y%T[4][1]" "yang2019S")))


(provide 'bibkey-tests)

;;; bibkey-tests.el ends here
;; Local Variables:
;; coding: utf-8
;; fill-column: 79
;; End:
