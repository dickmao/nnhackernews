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
  Then of-record unreads for "nnhackernews:news" is 186
  And I scan news
  Then of-record unreads for "nnhackernews:news" is 280
  When I go to word "nnhackernews:news"
  And I press "RET"
  Then I should be in buffer "*Summary nnhackernews:news*"
  And I go to word "DoreenMich"
  And I press "C-k"
  And prospective unreads for "nnhackernews:news" is 273
  And I scan news
  When I go to word "nnhackernews:news"
  And I press "RET"
  Then prospective unreads for "nnhackernews:news" is 273
  And of-record unreads for "nnhackernews:news" is 312
  And I press "q"
  And I dump buffer
  Then of-record unreads for "nnhackernews:news" is 303

@reply_nologin
Scenario: reply not having logged in yet
  When I go to word "nnhackernews:news"
  And I press "RET"
  Then I should be in buffer "*Summary nnhackernews:news*"
  And I go to word "LeifCarrot"
  And I press "r"
  Then I should be in buffer "*unsent followup to LeifCarrotson on news*"
  And I type "This is a test."
  And I press "C-c C-c"
  And I switch to buffer "*sent followup to LeifCarrotson on news*"
  Then I should be in buffer "*sent followup to LeifCarrotson on news*"

@delete
Scenario: delete
  When I switch to buffer "*Summary nnhackernews:news*"
  And I press "c y"
  Then I should be in buffer "*Group*"
  And I scan news
  When I go to word "nnhackernews:news"
  And I press "RET"
  Then I should be in buffer "*Summary nnhackernews:news*"
  And I go to word "dickmao"
  And I press "RET"
  And I press "S c"

@vote_reply_login
Scenario: vote (and login) and reply having already logged in
  Then I should be in buffer "*Summary nnhackernews:news*"
  And I go to word "ceejayoz"
  And I press "RET"
  And I press "R ="
  And I press "C-x o"
  Then I should see "Score: 0 +1"
  And I press "R 0"
  Then I should not see "Score: 0 +1"
  When I switch to buffer "*Summary nnhackernews:news*"
  And I press "r"
  Then I should be in buffer "*unsent followup to ceejayoz on news*"
  And I type "This is a test."
  And I press "C-c C-c"
  And I switch to buffer "*sent followup to ceejayoz on news*"
  Then I should be in buffer "*sent followup to ceejayoz on news*"

@submit
Scenario: submit a text which must be titled
  When I switch to buffer "*Summary nnhackernews:news*"
  And I press "a t"
  Then I should be in buffer "*unsent posting on news*"
  And I type "test baby test baby 123"
  And I press "M->"
  And I type "this is a test"
  And I press "C-c C-c"
  When I switch to buffer "*Messages*"
  Then I should not see "Couldn't send message via news"
