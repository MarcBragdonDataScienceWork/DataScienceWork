
import matplotlib.pyplot as plt
import numpy as np
import keras
from keras.layers import Conv2D, MaxPool2D, Flatten
from keras.layers import Dense, Dropout
#from xgboost import XGBClassifier
import seaborn as sn
from sklearn.metrics import accuracy_score
import log
import tensorflow as tf
from keras.preprocessing.image import ImageDataGenerator
	
logger = log.setup_custom_logger('CNN-Images')

# train_labels = keras.utils.np_utils.to_categorical(train_labels)
# test_labels = keras.utils.np_utils.to_categorical(test_labels)
logger.info("Creating Model")
model = keras.models.Sequential([
	#32 3x3 subregions
    keras.layers.Conv2D(32, (3, 3), padding="same", input_shape=[28, 28, 3]),
    #2x2 pool with with stripe of two so they don't overlap
    keras.layers.MaxPool2D((2,2)),
    #64 3x3 filters
    keras.layers.Conv2D(64, (3, 3), padding="same"),
    #pool again
    keras.layers.MaxPool2D((2,2)),
    #flatten before dense layer
    keras.layers.Flatten(),
    #dense neuron layer
    keras.layers.Dense(1024, activation='relu'),
    keras.layers.Dropout(0.5),
    #dense layer for output
    keras.layers.Dense(26, activation='softmax')
])

# model = keras.models.Sequential()
# model.add(keras.layers.Conv2D(32, (3,3), padding='same', activation='relu', input_shape=(28, 28, 3)))
# model.add(keras.layers.Conv2D(64, (3,3), padding='same', activation='relu'))
# model.add(keras.layers.MaxPooling2D(pool_size=(2, 2)))
# model.add(keras.layers.Dropout(0.3))
# model.add(keras.layers.Conv2D(128, (3,3), padding='same', activation='relu'))
# model.add(keras.layers.MaxPooling2D(pool_size=(2, 2)))
# model.add(keras.layers.Conv2D(128, (2,2), padding='same', activation='relu'))
# model.add(keras.layers.MaxPooling2D(pool_size=(2, 2)))
# model.add(keras.layers.Dropout(0.3))
# model.add(keras.layers.Flatten())
# model.add(keras.layers.Dense(1500, activation='relu'))
# model.add(keras.layers.Dropout(0.5))
# model.add(keras.layers.Dense(26, activation='softmax'))

logger.info("Compiling Model")
model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

'''
This turned out to be entirely necessary. On the computer I was running this on, attempts to
read the images into an array to run through the model would not fit in RAM and kept crashing
it slows the model down but with this it runs
'''
logger.info("Data Generator")
train_datagen = ImageDataGenerator(
        rescale=1./255,
        shear_range=0.2,
        zoom_range=0.2,
        horizontal_flip=True)

test_datagen = ImageDataGenerator(rescale=1./255)

training_set = train_datagen.flow_from_directory(
        '../data',
        target_size=(28, 28),
        batch_size=32,
        class_mode='categorical')

test_set = test_datagen.flow_from_directory(
        '../test',
        target_size=(28, 28),
        batch_size=32,
        class_mode='categorical')
logger.info("Fit Model")
history = model.fit_generator(
        training_set,
        steps_per_epoch=5000,
        epochs=10,
        validation_data = test_set,
        validation_steps = 500
      )

logger.info("Saving Model")
model.save('Trained_model.h5')

print(history.history.keys())
# summarize history for accuracy
plt.plot(history.history['acc'])
plt.plot(history.history['val_acc'])
plt.title('history accuracy')
plt.ylabel('accuracy')
plt.xlabel('epoch')
plt.legend(['train', 'test'], loc='upper left')
plt.show()
# summarize history for loss

plt.plot(history.history['loss'])
plt.plot(history.history['val_loss'])
plt.title('model loss')
plt.ylabel('loss')
plt.xlabel('epoch')
plt.legend(['train', 'test'], loc='upper left')
plt.show()


