---
editor_options: 
  markdown: 
    wrap: 72
---

# RReporting

Post trade Shiny app: looks at previous trades (file Trades.csv) and at
new trades recorded by IBKR and exported either:

1/ via TradeSimu Flex Query for Simu account

2/ via TradeLive Flex Query for Live account

See IBKR Personal web site - from Account Management Home -\> Reports
-\> Flex QUeries

Then allows to integrate those new trades into previous trades:

1.  Brand new trade-\> Use Open

2.  Add a new trade to an already opened trade -\> Use Adjust

3.  Close an existing trade -\> Use Close

After it is possible to save the updated Trades.csv file

**Missing features:**

None?
