import cv2
import numpy as np
from PIL import Image
from keras import models

#Load the saved model
model = models.load_model('../model/keras_model.h5')
video = cv2.VideoCapture(1)

while True:
        _, frame = video.read()

        #Convert the captured frame into RGB
        im = Image.fromarray(frame, 'RGB')

        im = im.resize((28,28))


        img_array = np.array(im)

        img_array = img_array/255

        img_array = np.expand_dims(img_array, axis=0)

        prediction = model.predict(img_array)        

        labels = np.argmax(prediction, axis=-1)

        # print("A : {} \n B: {} \n C: {} \n D: {} \n E: {} \n F: {} \n G: {} \n H: {} \n I: {} ".format(
        #         prediction[0][0],
        #         prediction[0][1],
        #         prediction[0][2],
        #         prediction[0][3],
        #         prediction[0][4],
        #         prediction[0][5],
        #         prediction[0][6],
        #         prediction[0][7],
        #         prediction[0][8],
        #         prediction[0][9]), end='\r')

        print("Current Prediction is : {} ".format(chr(labels + 96)), end='\r')
        classes = ['A', 'B','C', 'D','E', 'F']
        
        #cv2.imshow("Capturing", frame)
        key=cv2.waitKey(1)
        if key == ord('q'):
                break
video.release()
cv2.destroyAllWindows()
