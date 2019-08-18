@refresh_token
Scenario: start gnus
  Given gnus start
  And I dump buffer
