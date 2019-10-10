import pandas as pd
import numpy as np
import log
from textblob import TextBlob
from tqdm import tqdm
from multiprocessing import Pool

logger = log.setup_custom_logger('write_sentiment')

def get_data(filename):
	data = pd.read_csv(filename)
	return data

def clean_data(data):
	#Link the progress bar to pandas

	logger.info('Data Size Read : {}'.format(len(data)))
	data = data.dropna()
	logger.info('Size of Data after Dropping Nas is : {}'.format(len(data)))
	#data = data.drop('comment', axis=1)
	logger.info("Cleaning Data")
	data = data.drop('created_utc', axis=1)
	#data = data.drop('parent_comment', axis=1)
	data = data.drop('subreddit', axis=1)
	data = data.drop('date', axis=1)
	data = data.drop('author', axis=1)
	#including a vote vote_differential to see if that matters
	#data['vote_differential'] = data['ups'] - data['downs']
	data = data.drop('ups', axis=1)
	data = data.drop('downs', axis=1)
	data = data.drop('score', axis=1)

	return data

def add_sentiment_columns(data):
	tqdm.pandas()

	logger.info("Calculating Sentiment Shift")
	#Do inplace sentiment analysis for polarity
	try:
		data['sentiment_parent'] = data['parent_comment'].progress_apply(lambda comment: TextBlob(comment).sentiment.polarity)
		data['sentiment_child'] = data['comment'].progress_apply(lambda comment: TextBlob(comment).sentiment.polarity)
		data['sentiment_shift'] = np.absolute(data['sentiment_parent'] - data['sentiment_child'])
	except Exception as ex:
   		print(ex)
	return data



#This splits the data and runs the apply function in multicore mode
def run_parallel_pandas(data_frame, function, num_cores=1, num_parts=5):
	split_frame = np.array_split(data_frame, num_parts)
	pool = Pool(num_cores)
	return_data = pd.concat(pool.map(function, split_frame))
	pool.close()
	pool.join()
	return return_data

data = get_data('data/train-balanced-sarcasm.csv')
print(data.head(100))
data = clean_data(data)
data = add_sentiment_columns(data)
data.to_csv("output_sentiment.csv")