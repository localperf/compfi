#--scrape.py

from robobrowser import RoboBrowser

print "\timports OK"



browser = RoboBrowser(history = True)
browser.open ("http://FolioFn.com/")

print "\tinitial navigation ok"
