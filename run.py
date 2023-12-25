import numpy as np
from slime1 import slime1
from slime2 import slime2
from PIL import Image, ImageDraw
import pyopencl as cl
import cv2

def render1():
    S = slime1(interactive=False)
    w = 700; h = 700
    frames = 100
    x = np.zeros((frames,h,w), dtype=np.uint8)
    dth = .4
    step = 0.00525
    n = 1000000
    dr = 1.8 * (.008 / (200*200)) * 500000 / n
    diffuse = 0.1
    decay = 0.75
    print('rendering')
    d = S.main(dth, step, 0, dr, decay, diffuse,
               n, w, h, frames)
    print('saving')
    d.get(ary=x)
    out = [ Image.fromarray(np.dstack([f,f,f]), mode="RGB") for f in x ]

    out[0].save('slime-out.gif', save_all=True, duration=8, append_images=out[1:], loop=0)
    return out

def render2():
    print('opening')
    S = slime2(interactive=False)

    # params #
    fps = 30
    frames = fps*6

    dth = 0.5
    step = 0.00325
    n = 1000000
    dr = 3.500 * (.008 / (200*200)) * 500000 / (n+1)
    diffuse = 0.25
    decay = 0.95
    # end params #

    input_img = Image.open("inputs/mm1.jpg")
    w = input_img.size[0]
    h = input_img.size[1]
    chan = np.zeros((frames,h,w), dtype=np.uint8)
    input_img = np.array(input_img.getdata()).reshape(h, w, 3)
    input_img = np.array(input_img/255, dtype=np.float32)

    print('rendering')
    (cr,cg,cb) = S.main(input_img[:,:,0],input_img[:,:,1],input_img[:,:,2], dth, step, 0, dr, decay, diffuse, n, frames)

    print('imaging')
    x = np.zeros((frames,h,w,3), dtype=np.uint8)
    cr.get(ary=chan)
    x[:,:,:,2] = chan
    cg.get(ary=chan)
    x[:,:,:,1] = chan
    cb.get(ary=chan)
    x[:,:,:,0] = chan

    print('saving')
    size = (h,w)
    out = cv2.VideoWriter('slime-out.mp4', cv2.VideoWriter_fourcc(*'mp4v'), fps, (size[1], size[0]))
    for f in x:
        out.write(f)
    out.release()
    return out

    #out = [ Image.fromarray(f, mode="RGB") for f in x ]
    #out[0].save('slime-out.gif', save_all=True, duration=8, append_images=out[1:], loop=0)
    #return out

render2()