import functools
import numpy as np
from PIL import ImageGrab
import time
import cv2
import pyautogui
import threading
import keyboard
from pynput.keyboard import Key, Listener, KeyCode, Controller
import dirkeys as directkeys

cord_S = (210, 220, 435, 280)  # Real Game Coordinates of Pitch for S Key
cord_W = (210, 280, 435, 350)  # Real Game Coordinates of Pitch for W Key

left_s = (240, 210, 285, 280)
middle_s = (285, 210, 310, 280)
right_s = (310, 210, 380, 280)

left_w = (240, 280, 285, 330)
middle_w = (285, 280, 310, 330)
right_w = (310, 280, 380, 330)

timer = (96, 120, 105, 121)  # Timer Coordinates
ball = (548, 94, 549, 95)
player = (264, 88, 3280, 245)

Keyboard = Controller()


# Blue Circle [[[ 45 168 255]]]
# Pitch [[[172 169 155]]]


def find_sphere(image, key_area):
    if key_area == 's':
        found = False
        for i in image:
            for x in i:
                if x[2] > 230 and x[0] < 100:
                    found = True
        return found

    elif key_area == 'w':
        found = False
        for y in image:
            for j in y:
                if j[2] > 230 and j[0] < 100:
                    found = True
        return found


def timing(image, keys):
    while True:
        timing_key = np.array(ImageGrab.grab(bbox=image))
        if timing_key[0][0][1] != 37:
            if len(keys) == 1:
                Keyboard.press(Key.L_SHIFT)
                print(keys[0], "\n")
                break
            elif len(keys) == 3:
                pyautogui.press('shift')
                directkeys.PressKey(keys[1])
                directkeys.PressKey(keys[2])
                time.sleep(0.07)
                directkeys.ReleaseKey(keys[2])
                directkeys.ReleaseKey(keys[1])
                print(keys[0], keys[1], keys[2], "\n")
                break
            else:
                pyautogui.press('shift')
                directkeys.PressKey(keys[1])
                directkeys.PressKey(keys[2])
                directkeys.PressKey(keys[3])
                time.sleep(0.07)
                directkeys.ReleaseKey(keys[3])
                directkeys.ReleaseKey(keys[2])
                directkeys.ReleaseKey(keys[1])
                print(keys[0], keys[1], keys[2], keys[3], "\n")
                break


def figure_keys(img, bat, cord):
    for y in range(0, len(img)):
        for x in range(0, len(img[y])):
            if img[y][x][0] < 100 and img[y][x][2] > 230:
                blue = (cord_W[0] + x, cord_W[1] + y) if cord == 'w' else (cord_S[0] + x, cord_S[1] + y)
                if cord == 's':
                    # Shot Combination with S Key and Right Handed
                    if bat and (blue[0] in range(left_s[0], left_s[2])) and (blue[1] in range(left_s[1], left_s[3])):
                        # return "shift","s","down","left"
                        return directkeys.L_SHIFT, directkeys.S, directkeys.DOWN, directkeys.LEFT

                    elif bat and (blue[0] in range(middle_s[0], middle_s[2])) and (
                            blue[1] in range(middle_s[1], middle_s[3])):
                        # return "shift", "s", "down"
                        return directkeys.L_SHIFT, directkeys.S, directkeys.DOWN

                    # Shot Combination with S Key and Left Handed
                    elif not bat and (blue[0] in range(left_s[0], left_s[2])) and (
                            blue[1] in range(left_s[1], left_s[3])):
                        # return "s",
                        return directkeys.S,

                    elif not bat and (blue[0] in range(middle_s[0], middle_s[2])) and (
                            blue[1] in range(middle_s[1], middle_s[3])):
                        # return "shift", "s", "down"
                        return directkeys.L_SHIFT, directkeys.S, directkeys.DOWN

                    elif not bat and (blue[0] in range(right_s[0], right_s[2])) and (
                            blue[1] in range(right_s[1], right_s[3])):
                        # return "shift", "s", "down", "right"
                        return directkeys.L_SHIFT, directkeys.S, directkeys.DOWN, directkeys.RIGHT

                    else:
                        # return "s",
                        return directkeys.S,

                else:
                    # Shot Combination with W Key and Right Handed
                    if bat and (blue[0] in range(left_w[0], left_w[2])) and (blue[1] in range(left_w[1], left_w[3])):
                        # return "shift", "w", "up", "left"
                        return directkeys.L_SHIFT, directkeys.W, directkeys.UP, directkeys.LEFT

                    elif bat and (blue[0] in range(middle_s[0], middle_s[2])) and (
                            blue[1] in range(middle_s[1], middle_s[3])):
                        # return "shift", "w", "up", "right"
                        return directkeys.L_SHIFT, directkeys.W, directkeys.UP, directkeys.RIGHT

                    # Shot Combination with W Key and Left Handed
                    elif not bat and (blue[0] in range(left_s[0], left_s[2])) and (
                            blue[1] in range(left_s[1], left_s[3])):
                        # return "shift", "w", "up", "left"
                        return directkeys.L_SHIFT, directkeys.W, directkeys.UP, directkeys.LEFT

                    elif not bat and (blue[0] in range(middle_s[0], middle_s[2])) and (
                            blue[1] in range(middle_s[1], middle_s[3])):
                        # return "shift", "w", "up", "left"
                        return directkeys.L_SHIFT, directkeys.W, directkeys.UP, directkeys.LEFT

                    else:
                        # return "s",
                        return directkeys.S,
    #return "s",
    return directkeys.S,


class CountCalls:
    # the init needs to have the func as argument and stores it
    def __init__(self, func):
        functools.update_wrapper(self, func)
        self.func = func
        self.num_calls = 0

    # extend functionality, execute function, and return the result
    def __call__(self, *args, **kwargs):
        self.num_calls += 1
        print(f"Call {self.num_calls} of {self.func.__name__!r}")
        return self.func(*args, **kwargs)


class StartPrg(threading.Thread):
    def __init__(self, delay):
        super(StartPrg, self).__init__()
        self.delay = delay
        self.running = False
        self.program_running = True

    def start_clicking(self):
        self.running = True

    def stop_clicking(self):
        self.running = False

    def exit(self):
        self.stop_clicking()
        self.program_running = False

    def run(self):
        while self.program_running:
            while self.running:
                start_playing(1)


delay = 0.01
start_stop_key = KeyCode(char='.')
exit_key = KeyCode(char='/')
right_handed = False


@CountCalls
def start_playing(num):
    white_point = False
    while not white_point:
        image = np.array(ImageGrab.grab(bbox=ball))
        if image[0][0][0] > 200:
            print("Found Bowler")
            break

    img = cv2.cvtColor(np.array(ImageGrab.grab(bbox=player)), cv2.COLOR_BGR2RGB)
    rh = cv2.imread("RH.png")
    player_cords = pyautogui.locate(rh, img, confidence=0.7)
    right_hand = True if player_cords is not None else False

    find = False
    while not find:
        click_key_s = np.array(ImageGrab.grab(bbox=cord_S))
        find = find_sphere(image=click_key_s, key_area='s')
        if find:
            keys = figure_keys(click_key_s, right_hand, 's')
            timing(timer, keys)
            time.sleep(2)
            break


        click_key_w = np.array(ImageGrab.grab(bbox=cord_W))
        find = find_sphere(image=click_key_w, key_area='w')
        if find:
            keys = figure_keys(click_key_w, right_hand, 'w')
            timing(timer, keys)
            time.sleep(2)
            break


thread = StartPrg(delay)
thread.start()


def on_press(key):
    if key == start_stop_key:
        if thread.running:
            thread.stop_clicking()
        else:
            thread.start_clicking()
    elif key == exit_key:
        thread.exit()
        listener.stop()


with Listener(on_press=on_press) as listener:
    listener.join()
