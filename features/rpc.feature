@refresh_token
Scenario: start gnus
  Given gnus start
  And I dump buffer

@extant
Scenario: gnus-demon-scan-news while summary buffer open, then auto-rescore upon quitting summary buffer
  Given of-record unreads for "nnhackernews:news" is 94
  When I go to word "nnhackernews:news"
  And I press "RET"
  Then I should be in buffer "*Summary nnhackernews:news*"
  And I press "C-k"
  Then prospective unreads for "nnhackernews:news" is 93
  And I press "g"
