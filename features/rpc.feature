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
  And I scan news
  And I switch to buffer "*Summary nnhackernews:news*"
  Then of-record unreads for "nnhackernews:news" is 187
  And prospective unreads for "nnhackernews:news" is 93
  And I press "q"
  Then I should be in buffer "*Group*"
  And I dump buffer
  Then of-record unreads for "nnhackernews:news" is 186
  And I scan news
  Then of-record unreads for "nnhackernews:news" is 280
  When I go to word "nnhackernews:news"
  And I press "RET"
  And I go to word "DoreenMich"
  And I press "C-k"
  And prospective unreads for "nnhackernews:news" is 273
  And I scan news
  When I go to word "nnhackernews:news"
  And I press "RET"
  Then prospective unreads for "nnhackernews:news" is 273
  And of-record unreads for "nnhackernews:news" is 312
  And I press "q"
  Then of-record unreads for "nnhackernews:news" is 303

@reply_nologin
Scenario: reply not having logged in yet
  When I go to word "nnhackernews:news"
  And I press "RET"
  Then I should be in buffer "*Summary nnhackernews:news*"
  And I go to word "LeifCarrot"
  And I press "r"
  Then I should be in buffer "what what"
