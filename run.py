import numpy as np
from slime1 import slime1
from PIL import Image, ImageDraw
import pyopencl as cl

def render1():
    S = slime1(interactive=False)
    w = 500; h = 500
    frames = 200
    x = np.zeros((frames,h,w), dtype=np.uint8)
    dth = 1.4
    step = 0.00525
    n = 2000000
    dr = 1.8 * (.008 / (200*200)) * 500000 / n
    diffuse = 0.2
    decay = 0.75
    print('rendering')
    d = S.main(dth, step, 0, dr, decay, diffuse,
               n, w, h, frames)
    print('saving')
    d.get(ary=x)
    out = [ Image.fromarray(np.dstack([f,f,f]), mode="RGB") for f in x ]

    out[0].save('slime-out.gif', save_all=True, duration=8, append_images=out[1:], loop=0)
    return out

render1()