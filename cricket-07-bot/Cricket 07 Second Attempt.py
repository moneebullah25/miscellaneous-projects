import time
import numpy as np
from PIL import ImageGrab
from pynput.keyboard import Controller, KeyCode


cord_S = (275, 235, 375, 260) # Real Game Coordinates of Pitch for S Key
cord_W = (275, 265, 375, 350) # Real Game Coordinates of Pitch for W Key
timer = (96, 120, 103, 121) # Timer Coordinates

Keyboard = Controller()
# Blue Circle [[[ 45 168 255]]]
# Pitch [[[172 169 155]]]
button = []


def click_s(image):
    found = False
    for i in image:
        for x in i:
            if x[2] > 230 and x[0] < 100:
                button.insert(0, 's')
                found = True
    return found


def click_w(image):
    found = False
    for y in image:
        for j in y:
            if j[2] > 230 and j[0] < 100:
                button.insert(0, 'w')
                found = True
    return found


def timing(image):
    while True:
        timing_key = np.array(ImageGrab.grab(bbox=image))
        if timing_key[0][0][1] != 37:
            shot = True
            return shot


time.sleep(5)
while True:
    shot_time = False
    click_key_w = np.array(ImageGrab.grab(bbox=cord_W))
    find = click_w(image=click_key_w)
    if find:
        shot_time = timing(image=timer)
        if shot_time:
            Keyboard.press(key=KeyCode.from_char(button[0]))
            time.sleep(0.07)
            Keyboard.release(key=KeyCode.from_char(button[0]))
            time.sleep(0.07)

    shot_time = False
    click_key_s = np.array(ImageGrab.grab(bbox=cord_S))
    find = click_s(image=click_key_s)
    if find:
        shot_time = timing(image=timer)
        if shot_time:
            Keyboard.press(key=KeyCode.from_char(button[0]))
            time.sleep(0.07)
            Keyboard.release(key=KeyCode.from_char(button[0]))
            time.sleep(0.07)
