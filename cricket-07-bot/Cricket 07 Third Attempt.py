import time
import numpy as np
from PIL import ImageGrab
from pynput.keyboard import Controller, KeyCode

best_timer = (60, 120, 140, 121) # Image for calculating best coordinates for shot
start_x = 60 # To get the best point possible for shot timing
cord_S = (275, 235, 375, 260) # Real Game Coordinates of Pitch for S Key
cord_W = (275, 265, 375, 350) # Real Game Coordinates of Pitch for W Key

Keyboard = Controller()
# Blue Circle [[[ 45 168 255]]]
# Pitch [[[172 169 155]]]
button = []
# X-axis 60 - 140

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


def check_best_timer(start_cord_x, image):
    count_37 = 0
    count_not37 = 0
    found_37 = False
    for i in image:
        for x in i:
            if (x[1] != 37) and (found_37 == False):
                count_not37 += 1
            if x[1] == 37:
                count_37 += 1
                found_37 = True
    end_x = start_cord_x + count_not37 + count_37
    start_cord_x = start_cord_x + count_not37
    best_time = ((end_x + start_cord_x) / 2) - 2
    return best_time


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
        best_timer_image = np.array(ImageGrab.grab(bbox=best_timer))
        point = check_best_timer(start_x, best_timer_image)
        best_point = round(point) - 3
        print(best_point)
        timer = (best_point, 120, best_point + 1, 121)  # Timer Coordinates for Batting Gauge
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
        best_timer_image = np.array(ImageGrab.grab(bbox=best_timer))
        point = check_best_timer(start_x, best_timer_image)
        best_point = round(point) - 3
        print(best_point)
        timer = (best_point, 120, best_point + 1, 121)  # Timer Coordinates for Batting Gauge
        shot_time = timing(image=timer)
        if shot_time:
            Keyboard.press(key=KeyCode.from_char(button[0]))
            time.sleep(0.07)
            Keyboard.release(key=KeyCode.from_char(button[0]))
            time.sleep(0.07)
