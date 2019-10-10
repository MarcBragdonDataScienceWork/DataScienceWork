import statsmodels.api as stats
import pandas as pd
import numpy as np
import log


logger = log.setup_custom_logger('logistic_regression')


def get_data(filename):
    data = pd.read_csv(filename)
    logger.info('Data Size Read : {}'.format(len(data)))
    data = data.dropna()
    logger.info('Size of Data after Dropping Nas is : {}'.format(len(data)))
    return data

def regression(data):
    logger.debug(data.columns)
    label = np.array(data['label'])
    data = data.drop('label', axis = 1)
    feature_list = list(data.columns)
    features = np.array(data)
    y = label
    x = data

    x2 = stats.add_constant(x)
    model = stats.Logit(y, x2)
    model_fit = model.fit()
    print(model_fit.summary())

def clean_data(data):

    data = data.drop('comment', axis=1)
    data = data.drop('created_utc', axis=1)
    data = data.drop('parent_comment', axis=1)
    data = data.drop('subreddit', axis=1)
    data = data.drop('date', axis=1)
    data = data.drop('author', axis=1)
    data = data.drop('sentiment_parent', axis=1)
    data = data.drop('sentiment_child', axis=1)
    #data = data.drop('sentiment_shift', axis =1)
 #    sentiment_parent    -0.0861      0.073     -1.185      0.236      -0.229       0.056
	# sentiment_child      0.0223      0.070      0.321      0.748      -0.114       0.159
	# sentiment_shift 
 #    #including a vote vote_differential to see if that matters
    #data['vote_differential'] = data['ups'] - data['downs']
    #data = data.drop('ups', axis=1)
    #data = data.drop('downs', axis=1)
    data = data.drop('score', axis=1)
    return data


data = get_data('data/train-balanced-sarcasm.csv')
clean = clean_data(data)
results = regression(clean)
