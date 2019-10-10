import pandas as pd
import log
from text_processing import TextProcessing
from xgboost import XGBClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from sklearn.feature_extraction.text import CountVectorizer
import seaborn as sn
import matplotlib.pyplot as plt
from sklearn.naive_bayes import MultinomialNB
from sklearn.model_selection import GridSearchCV
import os
from sklearn.metrics import confusion_matrix, classification_report
from sklearn.svm import SVC
from sklearn.externals import joblib
from tqdm import tqdm
import random
from textblob import TextBlob
import numpy as np
from sklearn.linear_model import LogisticRegression
from keras.models import Sequential
from keras.layers import Dense, Dropout, Activation, Flatten
from keras.layers import Convolution2D, MaxPooling2D
import torch
from keras import backend as K


print(K.tensorflow_backend._get_available_gpus())
logger = log.setup_custom_logger('analysis')
percent_of_data = .5  # 1% of the lines
sentiment_threshold = 0
text_process = TextProcessing()

tqdm.pandas()

def get_data(filename):
    #keep the header, take random rows
    logger.info("Reading {} percent of the data.".format(percent_of_data * 100))
    data = pd.read_csv(filename,
                        header=0,
                        skiprows=lambda index: index>0 and random.random() > percent_of_data)
    logger.info('Data Size Read : {}'.format(len(data)))
    logger.info('Dropping NAs')
    data = data.dropna()
    logger.info('Size of Data after Dropping Nas is : {}'.format(len(data)))
    
    return data

def clean_data(data):

	logger.info("Average Sentiment Shift : {}".format(np.mean(data['sentiment_shift'])))
	logger.info("Data Size : {}".format(len(data)))
	logger.info("Taking only data with sentiment shift above {}".format(sentiment_threshold))

	data = data[data.sentiment_shift >= sentiment_threshold]
	logger.info("Size of Higher Sentiment Shift : {}".format(len(data)))

	logger.info("Tokening Comment")
	data['token_comment'] = data['comment'].progress_apply(text_process.tokenize_text, n_grams=1)
	#data['token_comment'] = data['token_comment'].progress_apply(text_process.remove_punctuation)
	logger.info("Make Lower Case Comment")
	data['token_comment'] = data['token_comment'].progress_apply(text_process.make_lower_case)
	logger.info("Removing Stop Words")
	data['token_comment'] = data['token_comment'].progress_apply(text_process.remove_stop_words)

	return data

def analyze_xg(data_x, data_y, data_weights):
	logger.info("Flattening Clean Data")
	flattened_data = text_process.flatten_list(data_x)
	vectorizer = CountVectorizer(binary=False, min_df=5)
	logger.info('Running Vectorizer on Data')
	vector = vectorizer.fit_transform(flattened_data)
	logger.info("Vector Shape is {}.".format(vector.shape))
	X_train, X_test, y_train, y_test = train_test_split(vector, data_y, test_size = 0.1, random_state=3)
		
	logger.info("X Train Shape is : {}.".format(X_train.shape))
	logger.info("X Test Shape is : {}.".format(X_test.shape))
	logger.info("Y Train Shape is : {}.".format(y_train.shape))
	logger.info("Y Test Shape is : {}.".format(y_test.shape))

	logger.info("Starting Learning Model")
	xg_model = XGBClassifier(learning_rate=.05, max_depth=4, gamma=0, booster= 'gbtree', nthreads = 8, sample_weight=data_weights, verbose=True)
	xg_model.fit(X_train, y_train)
	logger.info(xg_model)
	logger.info("Using Model For Predictions")
	y_pred = xg_model.predict(X_test)
	# evaluate predictions
	accuracy = accuracy_score(y_test, y_pred)
	print("Accuracy: {}".format(accuracy * 100.0))
	confusionmatrix = pd.crosstab(y_test, y_pred, rownames=['Actual'], colnames=['Predicted'])
	print(confusionmatrix)
	sn.heatmap(confusionmatrix, annot=True, fmt='g')
	plt.show()
def analyze_svm(data_x, data_y, data_weights):
	logger.info("Flattening Clean Data")
	flattened_data = text_process.flatten_list(data_x)
	vectorizer = CountVectorizer(binary=False, min_df=5)
	logger.info('Running Vectorizer on Data')
	vector = vectorizer.fit_transform(flattened_data)
	logger.info("Vector Shape is {}.".format(vector.shape))
	data_weight_train, data_weight_test, X_train, X_test, y_train, y_test = train_test_split(data_weights, vector, data_y, test_size = 0.33, random_state=3)
		
	logger.info("X Train Shape is : {}.".format(X_train.shape))
	logger.info("X Test Shape is : {}.".format(X_test.shape))
	logger.info("Y Train Shape is : {}.".format(y_train.shape))
	logger.info("Y Test Shape is : {}.".format(y_test.shape))

	logger.info("Starting Learning Model")
	svm = SVC(C=1.0, gamma="scale", kernel='linear', probability=True, verbose=3)
	logger.info("Calculating with Weights")
	svm.fit(X_train, y_train,sample_weight=data_weight_train)
	logger.info(svm)
	logger.info("Using Model For Predictions")
	y_pred = svm.predict(X_test)
	# evaluate predictions
	accuracy = accuracy_score(y_test, y_pred)
	print("Accuracy: {}".format(accuracy * 100.0))
	confusionmatrix = pd.crosstab(y_test, y_pred, rownames=['Actual'], colnames=['Predicted'])
	print(confusionmatrix)
	sn.heatmap(confusion_matrix, annot=True, fmt='g')
	plt.show()
	logger.info("Calculating without Weights")
	svm.fit(X_train, y_train)#,sample_weight=data_weight_train)
	logger.info(svm)
	logger.info("Using Model For Predictions")
	y_pred = svm.predict(X_test)
	# evaluate predictions
	accuracy = accuracy_score(y_test, y_pred)
	print("Accuracy: {}".format(accuracy * 100.0))
	confusionmatrix = pd.crosstab(y_test, y_pred, rownames=['Actual'], colnames=['Predicted'])
	print(confusionmatrix)


def analyze_log_reg(data_x, data_y, data_weights):
	logger.info("Flattening Clean Data")
	flattened_data = text_process.flatten_list(data_x)
	vectorizer = CountVectorizer(binary=False, min_df=5)
	logger.info('Running Vectorizer on Data')
	vector = vectorizer.fit_transform(flattened_data)
	logger.info("Vector Shape is {}.".format(vector.shape))
	data_weight_train, data_weight_test, X_train, X_test, y_train, y_test = train_test_split(data_weights, vector, data_y, test_size = 0.1, random_state=3)
		
	logger.info("X Train Shape is : {}.".format(X_train.shape))
	logger.info("X Test Shape is : {}.".format(X_test.shape))
	logger.info("Y Train Shape is : {}.".format(y_train.shape))
	logger.info("Y Test Shape is : {}.".format(y_test.shape))

	logger.info("Starting Learning Model")
	reg_model = LogisticRegression()

	logger.info("Calculating with Weights")
	reg_model.fit(X_train, y_train,sample_weight=data_weight_train)
	logger.info(reg_model)
	logger.info("Using Model For Predictions")
	y_pred = reg_model.predict(X_test)
	# evaluate predictions
	accuracy = accuracy_score(y_test, y_pred)
	print("Accuracy: {}".format(accuracy * 100.0))
	confusionmatrix = pd.crosstab(y_test, y_pred, rownames=['Actual'], colnames=['Predicted'])
	print(confusionmatrix)

	logger.info("Calculating without Weights")
	reg_model.fit(X_train, y_train)#,sample_weight=data_weight_train)
	logger.info(reg_model)
	logger.info("Using Model For Predictions")
	y_pred = reg_model.predict(X_test)
	# evaluate predictions
	accuracy = accuracy_score(y_test, y_pred)
	print("Accuracy: {}".format(accuracy * 100.0))
	confusionmatrix = pd.crosstab(y_test, y_pred, rownames=['Actual'], colnames=['Predicted'])
	print(confusionmatrix)
	sn.heatmap(confusionmatrix, annot=True, fmt='g')
	plt.show()

def analyze_nn(data_x, data_y, data_weights):
	logger.info("Flattening Clean Data")
	flattened_data = text_process.flatten_list(data_x)
	vectorizer = CountVectorizer(binary=False, min_df=5)
	logger.info('Running Vectorizer on Data')
	vector = vectorizer.fit_transform(flattened_data)
	logger.info("Vector Shape is {}.".format(vector.shape))
	data_weight_train, data_weight_test, X_train, X_test, y_train, y_test = train_test_split(data_weights, vector, data_y, test_size = 0.01, random_state=3)
		
	logger.info("X Train Shape is : {}.".format(X_train.shape))
	logger.info("X Test Shape is : {}.".format(X_test.shape))
	logger.info("Y Train Shape is : {}.".format(y_train.shape))
	logger.info("Y Test Shape is : {}.".format(y_test.shape))

	#set dimension size of data 
	input_dim = X_train.shape[1]

	model = Sequential()
	model.add(Dense(2500, activation='relu', input_dim=input_dim))
	model.add(Dense(512, activation='relu'))
	model.add(Dropout(0.50))
	
	model.add(Dense(1, activation='sigmoid'))
	model.compile(loss='binary_crossentropy', 
    	           optimizer='adam', 
	               metrics=['accuracy'])
	print(model.summary())
	history = model.fit(X_train, y_train,
                     epochs=25,
                     verbose=True,
                     validation_data=(X_test, y_test),
                     batch_size=128)
	loss, accuracy = model.evaluate(X_train, y_train, verbose=True)
	print("Training Accuracy: {:.4f}".format(accuracy))
	loss, accuracy = model.evaluate(X_test, y_test, verbose=True)
	print("Testing Accuracy:  {:.4f}".format(accuracy))




#print(data.head(100))
data = get_data('output_sentiment.csv')
data = clean_data(data)
#results = analyze_xg(data['token_comment'], data['label'], data['sentiment_shift'])
#results = analyze_svm(data['token_comment'], data['label'], data['sentiment_shift'])
#results = analyze_log_reg(data['token_comment'], data['label'], data['sentiment_shift'])
results = analyze_nn(data['token_comment'], data['label'], data['sentiment_shift'])