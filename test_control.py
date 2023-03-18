from src.control import *


#Start main code
pxs = 224
control = Control_Testing(
    IMG_W = pxs,
    IMG_H = pxs,
    latent_dim = 500,
    g=2
)
control.plot_Qtable()