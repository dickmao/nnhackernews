(require 'nnhackernews-test)

(ert-deftest nnhackernews-should-not-cache ()
  (should (string-match gnus-uncacheable-groups "nnhackernews:news")))

