from src.control import *

#Hyperparameter definitions
pxs = 400

#FLAGS definitions
summary = False
convolutional = True

#Start main code
control = RL(
    IMG_W = pxs,
    IMG_H = pxs,
    latent_dim = 500
)
if convolutional:
    control.createCAE(summary=summary)
else:
    control.createDAE(summary=summary)
control.loadAE()
control.testAE()

