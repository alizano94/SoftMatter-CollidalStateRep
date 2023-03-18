from src.control import *
import os

#Start main code
pxs = 224
control = Control_Testing(
    IMG_W = pxs,
    IMG_H = pxs,
    latent_dim = 500,
    g=2
)
coords = os.path.join(
    control.initial_states_path,
    '3.txt'
)
out_file = os.path.join(
    control.initial_states_path,
    '3.png'
)
control.plot_configuration(
    coords=coords,
    out_file=out_file
)