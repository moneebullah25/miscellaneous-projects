import time
import numpy as np
from PIL import ImageGrab
from pynput.keyboard import Key, Controller, KeyCode
Keyboard = Controller()

game_cord = (92, 120, 105, 121)
game_cord_S = (240, 225, 390, 280)
game_cord_W = (240, 285, 390, 350)


def bot():
    while True:
        timing = np.array(ImageGrab.grab(bbox=game_cord))
        if timing[0][0][1] != 37:
            Keyboard.press(key=KeyCode.from_char('s'))
            time.sleep(0.05)


time.sleep(5)
bot()

# Point(x=681, y=560)
# Blue Circle [[[ 45 168 255]]]
# Pitch [[[172 169 155]]]
