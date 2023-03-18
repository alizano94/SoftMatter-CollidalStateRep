from src.control import *

#Hyperparameter definitions
pxs = 224

#FLAGS definitions

#Start main code
control = RL(
    IMG_W = pxs,
    IMG_H = pxs,
    latent_dim = 500
)
control.createCAE(summary=False)
control.testAE()

