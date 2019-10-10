import glob
import log
from sklearn.preprocessing import LabelEncoder
import tensorflow as tf
from sklearn.model_selection import train_test_split
import pandas as pd
import numpy as np
import cv2
import pandas as pd

__author___ = "Marc Bragdon"

'''
Utility class to read in the images for the sign language project
'''
class ReadImages():
	'''
	Default image size to resize to
	'''
	image_size = (100, 100)
	channels = 3
	logger = log.setup_custom_logger('Read-Images')


	def make_letter_number(letter):
		return ord(letter) - 96
	'''
	Our labels are letters, because of course they are so
	we need to encode them to numerics 
	'''
	def encode_labels(self, labels):
		# integer_encoded = []
		# for label in labels:
		# 	self.logger.debug("Encoding label {}".format(label))
		# 	integer_encoded.append(self.make_letter_number(chr(label)))
		label_encoder = LabelEncoder()
		integer_encoded = label_encoder.fit_transform(labels)		
		return integer_encoded
	'''	
	Takes a file path in and reads the image file names 
	assigning a label based on what folder they are in as that
	is how our data is structured

	Argument : A file path with our images files in it
	Returns list of file paths and labels
	'''
	def get_file_names_and_labels(self, file_path):
		labels = []
		file_paths = []

		list_of_files = glob.iglob(file_path, recursive=True)

		for file in list_of_files:
			self.logger.debug('Attempting to Read {}'.format(file))
			label = file.split('/')[2]
			file_name = file.split('/')[3]
			self.logger.debug('Reading {} with label {}.'.format(file_name, label))
			labels.append(label)
			file_paths.append(file)

		return labels, file_paths

	'''
	Decode and read image vector data from the actual images
	and also resize to the "correct shape" which we are still trying to 
	figure out
	'''
	def read_and_preprocess_images(self, image_paths):
		#image_vectors = tf.zeros([len(image_paths),self.image_size[0], self.image_size[1], self.channels], dtype=tf.dtypes.int32)
		#images_vectors = np.empty([len(image_paths),self.image_size[0], self.image_size[1], self.channels])
		images_vectors = []
		for path in image_paths:
			self.logger.debug('Attempting Read on : {}'.format(path))
			#Read image file
			image = tf.read_file(path)
			#Decode it
			self.logger.debug('Decoding PNG')
			image = tf.image.decode_png(image, channels=3)
			#resize it
			self.logger.debug('Resizing image to {}'.format(self.image_size))
			image = tf.image.resize(image, self.image_size)
			self.logger.debug('Normalizing image.')
			image /= 255
			self.logger.debug('Image Shape : {}'.format(image.shape))
			self.logger.debug('Concating Image {}'.format(path))
			#self.logger.debug("Image Tensor : {}".format(image))
			images_vectors.append(image)


		return images_vectors
		#return tf.convert_to_tensor(images_vectors)
	'''
	Read the images in as numpy arrays
	'''
	def read_preprocess_images_numpy(self, image_paths):
		images_vectors = []
		for path in image_paths:
			self.logger.debug('Attempting Read on : {}'.format(path))
			#Read image file
			img = cv2.imread(path)		
			#resize it
			self.logger.debug('Resizing image to {}'.format(self.image_size))
			img = cv2.resize(img, self.image_size, interpolation=cv2.INTER_CUBIC)
			#change from BGR to RGB
			img = cv2.cvtColor(img, cv2.COLOR_BGR2RGB)
			img = img.astype(np.float32)
			self.logger.debug('Normalizing image.')
			img /= 255
			self.logger.debug('Image Shape : {}'.format(img.shape))
			self.logger.debug('Concating Image {}'.format(path))
			#self.logger.debug("Image Tensor : {}".format(img))
			images_vectors.append(img)
		return images_vectors


	'''
	Helper method to wrap it all together
	'''	
	def process_image_wrapper(self, file_path, test_train_split=0.1):
		self.logger.info("Getting File Names")
		labels, image_paths = self.get_file_names_and_labels(file_path)
		self.logger.info("Encoding Labels")
		labels = self.encode_labels(labels)
		self.logger.info("Processing Images")
		images = self.read_preprocess_images_numpy(image_paths)
		self.logger.info("Completed Preprocess")
		train_images, test_images, train_labels, test_labels = train_test_split(images, labels, test_size = test_train_split, random_state = 3)	

		return np.asarray(train_images), np.asarray(test_images), train_labels, test_labels

	def read_pixel_csv(self, file_path, test_train_split=0.1, reshape=False):
		self.logger.info("Reading Data File : {}".format(file_path))
		data = pd.read_csv(file_path)
		labels = np.array(data['label'])
	
		features = data.drop('label', axis=1)
		#Keep a list of feature names
		feature_list = list(features.columns)

		features = np.array(features)
		
		if(reshape):
			features = features.reshape(-1, 28, 28, 1)

		train_features, test_features, train_labels, test_labels = train_test_split(features, labels, test_size = test_train_split, random_state = 3)	
		
		train_features = train_features / 255.0
		test_features = test_features / 255.0
		return train_features, test_features, train_labels, test_labels, feature_list
		#return tf.data.Dataset.from_tensor_slices((labels, images))
		#return labels, images
data_dir = '../data/sign_mnist_train.csv'
#data_dir = 'sign_mnist_train.csv'
read_images = ReadImages()

train_features, test_features, train_labels, test_labels, feature_list= read_images.read_pixel_csv(data_dir, reshape=True)

print(train_features.shape)
print(train_labels.shape)