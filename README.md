# HESS
Haskell E-mail Scraper Spider

The `hess` program was built to have one purpose: find as many email-addresses as possible in as short a wall-clock time as possible.

Results:
```
$ time hess -f 10 <obfuscated seed> 
hess -f 10 <obfuscated seed>  3442.59s user 2198.10s system 591% cpu 15:53.34 total
$ wc -l mail.txt
356 mail.txt
```

What you do with the e-mail addresses afterward is not my problem.
This project was made just for fun.

## Extension Suggestions
### Url Database

The Url's that are found on a page are put in a 'queue'.
No effort has been put into making sure that this queue is emptied in a sensible manner.

I suggest to store the URL 'queue' in a database and have the fetchers ask for batches of Url's from a controller to this database.
The controller can then make sure that the same domain isn't crawled too often and maybe even keep track of which domains are better to find email-addresses on.


### Address verification

Some of the strings that show up in the output are definitely not email-addresses (for example: `github-logo@1200x1200.png`) but still conform to the RFC specification.

I suggest to validate email-addresses before outputting them.
Indications (not proof) of valitidy are:
- The domain exists and responds to pings
- The SMTP server affermatively answers a `RCPT` query
- https://www.webdigi.co.uk/blog/2009/how-to-check-if-an-email-address-exists-without-sending-an-email/
