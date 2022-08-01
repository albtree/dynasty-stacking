# dynasty-stacking

Help and credit to @EdnaBEASTmode for writing the code to pull the rosters and standings data from ffverse.
Credit to @Adeiko_FF for scraping and compiling sleeper league IDs

Used this code to run multivariate data analysis on whether stacking Quarterbacks, with Wide Receivers or Tight Ends from the same team improved season long outcomes in Dynasty leagues.

Limitations: Only one year of rosters and standings were pulled for this analysis. When rosters are pulled using ffscrapR, only the player’s current NFL team is pulled. Therefore, even if multiple years of data was pulled, the actual true number of stacks would be inaccurate given player movement e.g. Robert Woods would be attributed to be stacked with Ryan Tannehill as opposed to with Matthew Stafford. Although imperfect, given the results were far from statistical significance I don't think this is a major issue.

