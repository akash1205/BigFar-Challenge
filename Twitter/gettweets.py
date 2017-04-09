# -*- coding: utf-8 -*-

import sys,getopt,got,datetime,codecs

def main(argv):

	if len(argv) == 0:
		print ('You must pass some parameters. Use \"-h\" to help.')
		return

	if len(argv) == 1 and argv[0] == '-h':
		print ("""\nTo use this jar, you can pass the folowing attributes:
    username: Username of a specific twitter account (without @)
       since: The lower bound date (yyyy-mm-aa)
       until: The upper bound date (yyyy-mm-aa)
 querysearch: A query text to be matched
   maxtweets: The maximum number of tweets to retrieve

 \nExamples:
 # Example 1 - Get tweets by username [akash]
 python Exporter.py --username "akash" --maxtweets 1\n

 # Example 2 - Get tweets by query search [akash]
 python Exporter.py --querysearch "akash" --maxtweets 1\n

 # Example 3 - Get tweets by username and bound dates [akash, '2017-01-01', '2017-01-01']
 python Exporter.py --username "akash" --since 2017-01-01 --until 2017-03-01 --maxtweets 1\n

 # Example 4 - Get the last 10 top tweets by username
 python Exporter.py --username "akash" --maxtweets 10 --toptweets\n""")
		return

	try:
		opts, args = getopt.getopt(argv, "", ("username=", "since=", "until=", "querysearch=", "toptweets", "maxtweets="))

		tweetCriteria = got.manager.TweetCriteria()

		for opt,arg in opts:
			if opt == '--username':
				tweetCriteria.username = arg

			elif opt == '--since':
				tweetCriteria.since = arg

			elif opt == '--until':
				tweetCriteria.until = arg

			elif opt == '--querysearch':
				tweetCriteria.querySearch = arg

			elif opt == '--toptweets':
				tweetCriteria.topTweets = True

			elif opt == '--maxtweets':
				tweetCriteria.maxTweets = int(arg)


		outputFile = codecs.open("blackmoney.csv", "w+", "utf-16")

		outputFile.write('username;date;retweets;favorites;text;geo;mentions;hashtags;id;permalink')

		print ('Searching...\n')

		def receiveBuffer(tweets):
			for t in tweets:
				outputFile.write(('\n%s;%s;%d;%d;"%s";%s;%s;%s;"%s";%s' % (t.username, t.date.strftime("%Y-%m-%d %H:%M"), t.retweets, t.favorites, t.text, t.geo, t.mentions, t.hashtags, t.id, t.permalink)))
			outputFile.flush();
			print ('More %d saved on file...\n' % len(tweets))

		got.manager.TweetManager.getTweets(tweetCriteria, receiveBuffer)

	except arg:
		print ('Arguments parser error, try -h' + arg)
	finally:
		outputFile.close()
		print ('Done. Output file generated "file.csv".')

if __name__ == '__main__':
	main(sys.argv[1:])
