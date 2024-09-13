#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2023.2.3),
    on September 13, 2024, at 10:14
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

import psychopy
psychopy.useVersion('2023.2.3')


# --- Import packages ---
from psychopy import locale_setup
from psychopy import prefs
from psychopy import plugins
plugins.activatePlugins()
prefs.hardware['audioLib'] = 'ptb'
prefs.hardware['audioLatencyMode'] = '3'
from psychopy import sound, gui, visual, core, data, event, logging, clock, colors, layout
from psychopy.tools import environmenttools
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER, priority)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle, choice as randchoice
import os  # handy system and path functions
import sys  # to get file system encoding

import psychopy.iohub as io
from psychopy.hardware import keyboard

# --- Setup global variables (available in all functions) ---
# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
# Store info about the experiment session
psychopyVersion = '2023.2.3'
expName = 'CRL1e'  # from the Builder filename that created this script
expInfo = {
    'participant': f"{randint(0, 999999):06.0f}",
    'session': '001',
    'Age?': '',
    'Gender?': '',
    'Nationality?': '',
    'Highest Educational Degree?': '',
    'date': data.getDateStr(),  # add a simple timestamp
    'expName': expName,
    'psychopyVersion': psychopyVersion,
}


def showExpInfoDlg(expInfo):
    """
    Show participant info dialog.
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    
    Returns
    ==========
    dict
        Information about this experiment.
    """
    # temporarily remove keys which the dialog doesn't need to show
    poppedKeys = {
        'date': expInfo.pop('date', data.getDateStr()),
        'expName': expInfo.pop('expName', expName),
        'psychopyVersion': expInfo.pop('psychopyVersion', psychopyVersion),
    }
    # show participant info dialog
    dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
    if dlg.OK == False:
        core.quit()  # user pressed cancel
    # restore hidden keys
    expInfo.update(poppedKeys)
    # return expInfo
    return expInfo


def setupData(expInfo, dataDir=None):
    """
    Make an ExperimentHandler to handle trials and saving.
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    dataDir : Path, str or None
        Folder to save the data to, leave as None to create a folder in the current directory.    
    Returns
    ==========
    psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    """
    
    # data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
    if dataDir is None:
        dataDir = _thisDir
    filename = u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])
    # make sure filename is relative to dataDir
    if os.path.isabs(filename):
        dataDir = os.path.commonprefix([dataDir, filename])
        filename = os.path.relpath(filename, dataDir)
    
    # an ExperimentHandler isn't essential but helps with data saving
    thisExp = data.ExperimentHandler(
        name=expName, version='',
        extraInfo=expInfo, runtimeInfo=None,
        originPath='C:\\Users\\owner\\OneDrive - Yale University\\Collaborations\\Brooklyn_College\\categoricalReversalLerning\\exemplars_similarity\\fractal_similarity_lastrun.py',
        savePickle=True, saveWideText=True,
        dataFileName=dataDir + os.sep + filename, sortColumns='time'
    )
    thisExp.setPriority('thisRow.t', priority.CRITICAL)
    thisExp.setPriority('expName', priority.LOW)
    # return experiment handler
    return thisExp


def setupLogging(filename):
    """
    Setup a log file and tell it what level to log at.
    
    Parameters
    ==========
    filename : str or pathlib.Path
        Filename to save log file and data files as, doesn't need an extension.
    
    Returns
    ==========
    psychopy.logging.LogFile
        Text stream to receive inputs from the logging system.
    """
    # this outputs to the screen, not a file
    logging.console.setLevel(logging.EXP)
    # save a log file for detail verbose info
    logFile = logging.LogFile(filename+'.log', level=logging.EXP)
    
    return logFile


def setupWindow(expInfo=None, win=None):
    """
    Setup the Window
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    win : psychopy.visual.Window
        Window to setup - leave as None to create a new window.
    
    Returns
    ==========
    psychopy.visual.Window
        Window in which to run this experiment.
    """
    if win is None:
        # if not given a window to setup, make one
        win = visual.Window(
            size=[1536, 864], fullscr=True, screen=0,
            winType='pyglet', allowStencil=False,
            monitor='testMonitor', color=[0,0,0], colorSpace='rgb',
            backgroundImage='', backgroundFit='none',
            blendMode='avg', useFBO=True,
            units='height'
        )
        if expInfo is not None:
            # store frame rate of monitor if we can measure it
            expInfo['frameRate'] = win.getActualFrameRate()
    else:
        # if we have a window, just set the attributes which are safe to set
        win.color = [0,0,0]
        win.colorSpace = 'rgb'
        win.backgroundImage = ''
        win.backgroundFit = 'none'
        win.units = 'height'
    win.mouseVisible = False
    win.hideMessage()
    return win


def setupInputs(expInfo, thisExp, win):
    """
    Setup whatever inputs are available (mouse, keyboard, eyetracker, etc.)
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    win : psychopy.visual.Window
        Window in which to run this experiment.
    Returns
    ==========
    dict
        Dictionary of input devices by name.
    """
    # --- Setup input devices ---
    inputs = {}
    ioConfig = {}
    
    # Setup iohub keyboard
    ioConfig['Keyboard'] = dict(use_keymap='psychopy')
    
    ioSession = '1'
    if 'session' in expInfo:
        ioSession = str(expInfo['session'])
    ioServer = io.launchHubServer(window=win, **ioConfig)
    eyetracker = None
    
    # create a default keyboard (e.g. to check for escape)
    defaultKeyboard = keyboard.Keyboard(backend='iohub')
    # return inputs dict
    return {
        'ioServer': ioServer,
        'defaultKeyboard': defaultKeyboard,
        'eyetracker': eyetracker,
    }

def pauseExperiment(thisExp, inputs=None, win=None, timers=[], playbackComponents=[]):
    """
    Pause this experiment, preventing the flow from advancing to the next routine until resumed.
    
    Parameters
    ==========
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    inputs : dict
        Dictionary of input devices by name.
    win : psychopy.visual.Window
        Window for this experiment.
    timers : list, tuple
        List of timers to reset once pausing is finished.
    playbackComponents : list, tuple
        List of any components with a `pause` method which need to be paused.
    """
    # if we are not paused, do nothing
    if thisExp.status != PAUSED:
        return
    
    # pause any playback components
    for comp in playbackComponents:
        comp.pause()
    # prevent components from auto-drawing
    win.stashAutoDraw()
    # run a while loop while we wait to unpause
    while thisExp.status == PAUSED:
        # make sure we have a keyboard
        if inputs is None:
            inputs = {
                'defaultKeyboard': keyboard.Keyboard(backend='ioHub')
            }
        # check for quit (typically the Esc key)
        if inputs['defaultKeyboard'].getKeys(keyList=['escape']):
            endExperiment(thisExp, win=win, inputs=inputs)
        # flip the screen
        win.flip()
    # if stop was requested while paused, quit
    if thisExp.status == FINISHED:
        endExperiment(thisExp, inputs=inputs, win=win)
    # resume any playback components
    for comp in playbackComponents:
        comp.play()
    # restore auto-drawn components
    win.retrieveAutoDraw()
    # reset any timers
    for timer in timers:
        timer.reset()


def run(expInfo, thisExp, win, inputs, globalClock=None, thisSession=None):
    """
    Run the experiment flow.
    
    Parameters
    ==========
    expInfo : dict
        Information about this experiment, created by the `setupExpInfo` function.
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    psychopy.visual.Window
        Window in which to run this experiment.
    inputs : dict
        Dictionary of input devices by name.
    globalClock : psychopy.core.clock.Clock or None
        Clock to get global time from - supply None to make a new one.
    thisSession : psychopy.session.Session or None
        Handle of the Session object this experiment is being run from, if any.
    """
    # mark experiment as started
    thisExp.status = STARTED
    # make sure variables created by exec are available globally
    exec = environmenttools.setExecEnvironment(globals())
    # get device handles from dict of input devices
    ioServer = inputs['ioServer']
    defaultKeyboard = inputs['defaultKeyboard']
    eyetracker = inputs['eyetracker']
    # make sure we're running in the directory for this experiment
    os.chdir(_thisDir)
    # get filename from ExperimentHandler for convenience
    filename = thisExp.dataFileName
    frameTolerance = 0.001  # how close to onset before 'same' frame
    endExpNow = False  # flag for 'escape' or other condition => quit the exp
    # get frame duration from frame rate in expInfo
    if 'frameRate' in expInfo and expInfo['frameRate'] is not None:
        frameDur = 1.0 / round(expInfo['frameRate'])
    else:
        frameDur = 1.0 / 60.0  # could not measure, so guess
    
    # Start Code - component code to be run after the window creation
    
    # --- Initialize components for Routine "infCons" ---
    informed_consent = visual.TextStim(win=win, name='informed_consent',
        text='*** INFORMED CONSENT ***\n\nThis research investigates the psychological processes used when people learn to identify objects in the world. You are being asked to participate in this research study because you are a normal healthy adult, and we wish to better understand basic learning processes in your population. The purpose of this research is to gain more knowledge about the cognitive processes involved in simple forms of associative learning.\n\nIf you agree to participate, we will ask you to perform in a simple computer task that will last approximately 12 minutes. In this task, you will see a series of abstract images presented individually on the screen and your task will be to learn to choose one of two response options for each image.  Also, you will be asked to respond quickly and accurately on your computer keyboard when the image appears.\n\n• Risks/Discomforts: There are no risks for participating in this study beyond those associated with normal computer use over a 15 min period.\n\n• Benefits: This research is not designed to directly benefit you, but your help with this study will advance basic science on the cognitive processes involved in predictive learning in normal healthy individuals. Ultimately, this research could lead to a better understanding of some of the associative learning processes that are negatively impacted by various psychological conditions (such as aging, dementia, etc).\n\n• Confidentiality: This study does not collect identifying information, and all data collected will remain anonymous. We will ask about your gender, age, and nationality, and we will record your performance in the task itself. However, this information will not be linked directly to any individual participant. The data we obtain will be stored indefinitely and may be shared publicly via online repositories for findings that are ultimately published.\n\nYour participation in this research is completely voluntary, and you will be able to stop at any time without penalty. If you have any questions, you can contact: Andrew R. Delamater (andrewd@brooklyn.cuny.edu). If you have any questions about your rights as a research participant or if you would like to talk to someone other than the researcher, you can contact CUNY Research Compliance Administrator at 646-664-8918 or HRPP@cuny.edu.\n\nIf you wish to participate in the study, please press the spacebar for additional instructions.',
        font='Open Sans',
        pos=(0, 0), height=0.02, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    resp_consent = keyboard.Keyboard()
    
    # --- Initialize components for Routine "instructions" ---
    text_instructions = visual.TextStim(win=win, name='text_instructions',
        text='In this experiment, you will be presented with a series of abstract images that represent the molecular structure of various natural objects. Your task will be to learn from which of two regions in the world these objects come (Northern Hemisphere or Southern Hemisphere).\n\nMore specifically, you will see 1 of 8 different abstract images at a time and be asked to indicate whether you think that image reflects an object taken from the Northern or Southern Hemisphere. Choose the Left Arrow Key for Northern or the Right Arrow Key for Southern Hemisphere. At first, you will need to guess, but you will be provided with feedback after your answer to help you learn which objects come from Northern or Southern Hemispheres.\n\nYour response times are also important. Please make your response choices as quickly, but also as accurately, as you can. Your feedback will display the time (in sec) that it took for you to reach your decision, and also if your choice was correct (with a high pitch sound) or not (low pitch sound).\n\nThere will be a break halfway through.\n\nPress the space bar when you are ready to begin.',
        font='Open Sans',
        pos=(0, 0), height=0.02, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    resp_instructions = keyboard.Keyboard()
    
    # --- Initialize components for Routine "nextTask" ---
    next_task_ready = visual.TextStim(win=win, name='next_task_ready',
        text='You will now be presented with a new set of objects to learn about.\n\nPress the "Space Bar" when you are Ready to Begin (or Resume).',
        font='Open Sans',
        pos=(0, 0), height=0.02, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    ready_task = keyboard.Keyboard()
    
    # --- Initialize components for Routine "fixation" ---
    text_fixation = visual.TextStim(win=win, name='text_fixation',
        text='+',
        font='Open Sans',
        pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-1.0);
    
    # --- Initialize components for Routine "stim" ---
    stimulusLeft = visual.ImageStim(
        win=win,
        name='stimulusLeft', 
        image='default.png', mask=None, anchor='center',
        ori=0.0, pos=(0, -0.25), size=(0.35, 0.3),
        color=[1,1,1], colorSpace='rgb', opacity=1.0,
        flipHoriz=False, flipVert=False,
        texRes=128.0, interpolate=True, depth=0.0)
    stimulusRight = visual.ImageStim(
        win=win,
        name='stimulusRight', 
        image='default.png', mask=None, anchor='center',
        ori=0.0, pos=(0, 0.25), size=(0.35, 0.35),
        color=[1,1,1], colorSpace='rgb', opacity=None,
        flipHoriz=False, flipVert=False,
        texRes=128.0, interpolate=True, depth=-1.0)
    slider = visual.Slider(win=win, name='slider',
        startValue=None, size=(1.0, 0.05), pos=(0, 0), units=win.units,
        labels=None, ticks=(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), granularity=0.0,
        style='rating', styleTweaks=(), opacity=None,
        labelColor='LightGray', markerColor='Red', lineColor='White', colorSpace='rgb',
        font='Open Sans', labelHeight=0.05,
        flip=True, ori=0.0, depth=-2, readOnly=False)
    veryDifferent = visual.TextStim(win=win, name='veryDifferent',
        text='Very Distinct',
        font='Open Sans',
        pos=(-0.7, 0.05), height=0.05, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-3.0);
    verySimilar = visual.TextStim(win=win, name='verySimilar',
        text='Very Similar',
        font='Open Sans',
        pos=(0.7, 0.05), height=0.05, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=-4.0);
    
    # --- Initialize components for Routine "end" ---
    text_end = visual.TextStim(win=win, name='text_end',
        text='Thank you for your participation.\n\nPress the Space Bar to advance, but then Please wait until you see the green "Thank-You" message and click OK to exit.',
        font='Open Sans',
        pos=(0, 0), height=0.025, wrapWidth=None, ori=0.0, 
        color='white', colorSpace='rgb', opacity=None, 
        languageStyle='LTR',
        depth=0.0);
    resp_end = keyboard.Keyboard()
    
    # create some handy timers
    if globalClock is None:
        globalClock = core.Clock()  # to track the time since experiment started
    if ioServer is not None:
        ioServer.syncClock(globalClock)
    logging.setDefaultClock(globalClock)
    routineTimer = core.Clock()  # to track time remaining of each (possibly non-slip) routine
    win.flip()  # flip window to reset last flip timer
    # store the exact time the global clock started
    expInfo['expStart'] = data.getDateStr(format='%Y-%m-%d %Hh%M.%S.%f %z', fractionalSecondDigits=6)
    
    # --- Prepare to start Routine "infCons" ---
    continueRoutine = True
    # update component parameters for each repeat
    thisExp.addData('infCons.started', globalClock.getTime())
    resp_consent.keys = []
    resp_consent.rt = []
    _resp_consent_allKeys = []
    # keep track of which components have finished
    infConsComponents = [informed_consent, resp_consent]
    for thisComponent in infConsComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "infCons" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *informed_consent* updates
        
        # if informed_consent is starting this frame...
        if informed_consent.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            informed_consent.frameNStart = frameN  # exact frame index
            informed_consent.tStart = t  # local t and not account for scr refresh
            informed_consent.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(informed_consent, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'informed_consent.started')
            # update status
            informed_consent.status = STARTED
            informed_consent.setAutoDraw(True)
        
        # if informed_consent is active this frame...
        if informed_consent.status == STARTED:
            # update params
            pass
        
        # *resp_consent* updates
        waitOnFlip = False
        
        # if resp_consent is starting this frame...
        if resp_consent.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            resp_consent.frameNStart = frameN  # exact frame index
            resp_consent.tStart = t  # local t and not account for scr refresh
            resp_consent.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(resp_consent, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'resp_consent.started')
            # update status
            resp_consent.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(resp_consent.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(resp_consent.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if resp_consent.status == STARTED and not waitOnFlip:
            theseKeys = resp_consent.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
            _resp_consent_allKeys.extend(theseKeys)
            if len(_resp_consent_allKeys):
                resp_consent.keys = _resp_consent_allKeys[-1].name  # just the last key pressed
                resp_consent.rt = _resp_consent_allKeys[-1].rt
                resp_consent.duration = _resp_consent_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if defaultKeyboard.getKeys(keyList=["escape"]):
            thisExp.status = FINISHED
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, inputs=inputs, win=win)
            return
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in infConsComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "infCons" ---
    for thisComponent in infConsComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    thisExp.addData('infCons.stopped', globalClock.getTime())
    # check responses
    if resp_consent.keys in ['', [], None]:  # No response was made
        resp_consent.keys = None
    thisExp.addData('resp_consent.keys',resp_consent.keys)
    if resp_consent.keys != None:  # we had a response
        thisExp.addData('resp_consent.rt', resp_consent.rt)
        thisExp.addData('resp_consent.duration', resp_consent.duration)
    thisExp.nextEntry()
    # the Routine "infCons" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # --- Prepare to start Routine "instructions" ---
    continueRoutine = True
    # update component parameters for each repeat
    thisExp.addData('instructions.started', globalClock.getTime())
    resp_instructions.keys = []
    resp_instructions.rt = []
    _resp_instructions_allKeys = []
    # keep track of which components have finished
    instructionsComponents = [text_instructions, resp_instructions]
    for thisComponent in instructionsComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "instructions" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_instructions* updates
        
        # if text_instructions is starting this frame...
        if text_instructions.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_instructions.frameNStart = frameN  # exact frame index
            text_instructions.tStart = t  # local t and not account for scr refresh
            text_instructions.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_instructions, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'text_instructions.started')
            # update status
            text_instructions.status = STARTED
            text_instructions.setAutoDraw(True)
        
        # if text_instructions is active this frame...
        if text_instructions.status == STARTED:
            # update params
            pass
        
        # *resp_instructions* updates
        waitOnFlip = False
        
        # if resp_instructions is starting this frame...
        if resp_instructions.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            resp_instructions.frameNStart = frameN  # exact frame index
            resp_instructions.tStart = t  # local t and not account for scr refresh
            resp_instructions.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(resp_instructions, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'resp_instructions.started')
            # update status
            resp_instructions.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(resp_instructions.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(resp_instructions.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if resp_instructions.status == STARTED and not waitOnFlip:
            theseKeys = resp_instructions.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
            _resp_instructions_allKeys.extend(theseKeys)
            if len(_resp_instructions_allKeys):
                resp_instructions.keys = _resp_instructions_allKeys[-1].name  # just the last key pressed
                resp_instructions.rt = _resp_instructions_allKeys[-1].rt
                resp_instructions.duration = _resp_instructions_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if defaultKeyboard.getKeys(keyList=["escape"]):
            thisExp.status = FINISHED
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, inputs=inputs, win=win)
            return
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in instructionsComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "instructions" ---
    for thisComponent in instructionsComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    thisExp.addData('instructions.stopped', globalClock.getTime())
    # check responses
    if resp_instructions.keys in ['', [], None]:  # No response was made
        resp_instructions.keys = None
    thisExp.addData('resp_instructions.keys',resp_instructions.keys)
    if resp_instructions.keys != None:  # we had a response
        thisExp.addData('resp_instructions.rt', resp_instructions.rt)
        thisExp.addData('resp_instructions.duration', resp_instructions.duration)
    thisExp.nextEntry()
    # the Routine "instructions" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # set up handler to look after randomisation of conditions etc
    task = data.TrialHandler(nReps=1.0, method='fullRandom', 
        extraInfo=expInfo, originPath=-1,
        trialList=data.importConditions('design_task.csv'),
        seed=None, name='task')
    thisExp.addLoop(task)  # add the loop to the experiment
    thisTask = task.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisTask.rgb)
    if thisTask != None:
        for paramName in thisTask:
            globals()[paramName] = thisTask[paramName]
    
    for thisTask in task:
        currentLoop = task
        thisExp.timestampOnFlip(win, 'thisRow.t')
        # pause experiment here if requested
        if thisExp.status == PAUSED:
            pauseExperiment(
                thisExp=thisExp, 
                inputs=inputs, 
                win=win, 
                timers=[routineTimer], 
                playbackComponents=[]
        )
        # abbreviate parameter names if possible (e.g. rgb = thisTask.rgb)
        if thisTask != None:
            for paramName in thisTask:
                globals()[paramName] = thisTask[paramName]
        
        # --- Prepare to start Routine "nextTask" ---
        continueRoutine = True
        # update component parameters for each repeat
        thisExp.addData('nextTask.started', globalClock.getTime())
        ready_task.keys = []
        ready_task.rt = []
        _ready_task_allKeys = []
        # keep track of which components have finished
        nextTaskComponents = [next_task_ready, ready_task]
        for thisComponent in nextTaskComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        frameN = -1
        
        # --- Run Routine "nextTask" ---
        routineForceEnded = not continueRoutine
        while continueRoutine:
            # get current time
            t = routineTimer.getTime()
            tThisFlip = win.getFutureFlipTime(clock=routineTimer)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *next_task_ready* updates
            
            # if next_task_ready is starting this frame...
            if next_task_ready.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                next_task_ready.frameNStart = frameN  # exact frame index
                next_task_ready.tStart = t  # local t and not account for scr refresh
                next_task_ready.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(next_task_ready, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'next_task_ready.started')
                # update status
                next_task_ready.status = STARTED
                next_task_ready.setAutoDraw(True)
            
            # if next_task_ready is active this frame...
            if next_task_ready.status == STARTED:
                # update params
                pass
            
            # *ready_task* updates
            waitOnFlip = False
            
            # if ready_task is starting this frame...
            if ready_task.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                ready_task.frameNStart = frameN  # exact frame index
                ready_task.tStart = t  # local t and not account for scr refresh
                ready_task.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(ready_task, 'tStartRefresh')  # time at next scr refresh
                # add timestamp to datafile
                thisExp.timestampOnFlip(win, 'ready_task.started')
                # update status
                ready_task.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(ready_task.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(ready_task.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if ready_task.status == STARTED and not waitOnFlip:
                theseKeys = ready_task.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
                _ready_task_allKeys.extend(theseKeys)
                if len(_ready_task_allKeys):
                    ready_task.keys = _ready_task_allKeys[-1].name  # just the last key pressed
                    ready_task.rt = _ready_task_allKeys[-1].rt
                    ready_task.duration = _ready_task_allKeys[-1].duration
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if defaultKeyboard.getKeys(keyList=["escape"]):
                thisExp.status = FINISHED
            if thisExp.status == FINISHED or endExpNow:
                endExperiment(thisExp, inputs=inputs, win=win)
                return
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                routineForceEnded = True
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in nextTaskComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # --- Ending Routine "nextTask" ---
        for thisComponent in nextTaskComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        thisExp.addData('nextTask.stopped', globalClock.getTime())
        # check responses
        if ready_task.keys in ['', [], None]:  # No response was made
            ready_task.keys = None
        task.addData('ready_task.keys',ready_task.keys)
        if ready_task.keys != None:  # we had a response
            task.addData('ready_task.rt', ready_task.rt)
            task.addData('ready_task.duration', ready_task.duration)
        # the Routine "nextTask" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # set up handler to look after randomisation of conditions etc
        trials = data.TrialHandler(nReps=1.0, method='random', 
            extraInfo=expInfo, originPath=-1,
            trialList=data.importConditions('design_trials.csv', selection=taskRows),
            seed=None, name='trials')
        thisExp.addLoop(trials)  # add the loop to the experiment
        thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
        # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
        if thisTrial != None:
            for paramName in thisTrial:
                globals()[paramName] = thisTrial[paramName]
        
        for thisTrial in trials:
            currentLoop = trials
            thisExp.timestampOnFlip(win, 'thisRow.t')
            # pause experiment here if requested
            if thisExp.status == PAUSED:
                pauseExperiment(
                    thisExp=thisExp, 
                    inputs=inputs, 
                    win=win, 
                    timers=[routineTimer], 
                    playbackComponents=[]
            )
            # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
            if thisTrial != None:
                for paramName in thisTrial:
                    globals()[paramName] = thisTrial[paramName]
            
            # --- Prepare to start Routine "fixation" ---
            continueRoutine = True
            # update component parameters for each repeat
            thisExp.addData('fixation.started', globalClock.getTime())
            # Run 'Begin Routine' code from code_1
            jitter = random() * (1.0)
            jitter = round(jitter, 1) + 0.5
            thisExp.addData('ITI', jitter)
            # keep track of which components have finished
            fixationComponents = [text_fixation]
            for thisComponent in fixationComponents:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "fixation" ---
            routineForceEnded = not continueRoutine
            while continueRoutine:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *text_fixation* updates
                
                # if text_fixation is starting this frame...
                if text_fixation.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    text_fixation.frameNStart = frameN  # exact frame index
                    text_fixation.tStart = t  # local t and not account for scr refresh
                    text_fixation.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(text_fixation, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'text_fixation.started')
                    # update status
                    text_fixation.status = STARTED
                    text_fixation.setAutoDraw(True)
                
                # if text_fixation is active this frame...
                if text_fixation.status == STARTED:
                    # update params
                    pass
                
                # if text_fixation is stopping this frame...
                if text_fixation.status == STARTED:
                    # is it time to stop? (based on global clock, using actual start)
                    if tThisFlipGlobal > text_fixation.tStartRefresh + jitter-frameTolerance:
                        # keep track of stop time/frame for later
                        text_fixation.tStop = t  # not accounting for scr refresh
                        text_fixation.frameNStop = frameN  # exact frame index
                        # add timestamp to datafile
                        thisExp.timestampOnFlip(win, 'text_fixation.stopped')
                        # update status
                        text_fixation.status = FINISHED
                        text_fixation.setAutoDraw(False)
                
                # check for quit (typically the Esc key)
                if defaultKeyboard.getKeys(keyList=["escape"]):
                    thisExp.status = FINISHED
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, inputs=inputs, win=win)
                    return
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in fixationComponents:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "fixation" ---
            for thisComponent in fixationComponents:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            thisExp.addData('fixation.stopped', globalClock.getTime())
            # the Routine "fixation" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            
            # --- Prepare to start Routine "stim" ---
            continueRoutine = True
            # update component parameters for each repeat
            thisExp.addData('stim.started', globalClock.getTime())
            stimulusLeft.setImage(exemplarLeft)
            stimulusRight.setImage(exemplarRight)
            slider.reset()
            # keep track of which components have finished
            stimComponents = [stimulusLeft, stimulusRight, slider, veryDifferent, verySimilar]
            for thisComponent in stimComponents:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            frameN = -1
            
            # --- Run Routine "stim" ---
            routineForceEnded = not continueRoutine
            while continueRoutine:
                # get current time
                t = routineTimer.getTime()
                tThisFlip = win.getFutureFlipTime(clock=routineTimer)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *stimulusLeft* updates
                
                # if stimulusLeft is starting this frame...
                if stimulusLeft.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    stimulusLeft.frameNStart = frameN  # exact frame index
                    stimulusLeft.tStart = t  # local t and not account for scr refresh
                    stimulusLeft.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(stimulusLeft, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'stimulusLeft.started')
                    # update status
                    stimulusLeft.status = STARTED
                    stimulusLeft.setAutoDraw(True)
                
                # if stimulusLeft is active this frame...
                if stimulusLeft.status == STARTED:
                    # update params
                    pass
                
                # *stimulusRight* updates
                
                # if stimulusRight is starting this frame...
                if stimulusRight.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    stimulusRight.frameNStart = frameN  # exact frame index
                    stimulusRight.tStart = t  # local t and not account for scr refresh
                    stimulusRight.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(stimulusRight, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'stimulusRight.started')
                    # update status
                    stimulusRight.status = STARTED
                    stimulusRight.setAutoDraw(True)
                
                # if stimulusRight is active this frame...
                if stimulusRight.status == STARTED:
                    # update params
                    pass
                
                # *slider* updates
                
                # if slider is starting this frame...
                if slider.status == NOT_STARTED and tThisFlip >= 1-frameTolerance:
                    # keep track of start time/frame for later
                    slider.frameNStart = frameN  # exact frame index
                    slider.tStart = t  # local t and not account for scr refresh
                    slider.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(slider, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'slider.started')
                    # update status
                    slider.status = STARTED
                    slider.setAutoDraw(True)
                
                # if slider is active this frame...
                if slider.status == STARTED:
                    # update params
                    pass
                
                # Check slider for response to end Routine
                if slider.getRating() is not None and slider.status == STARTED:
                    continueRoutine = False
                
                # *veryDifferent* updates
                
                # if veryDifferent is starting this frame...
                if veryDifferent.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    veryDifferent.frameNStart = frameN  # exact frame index
                    veryDifferent.tStart = t  # local t and not account for scr refresh
                    veryDifferent.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(veryDifferent, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'veryDifferent.started')
                    # update status
                    veryDifferent.status = STARTED
                    veryDifferent.setAutoDraw(True)
                
                # if veryDifferent is active this frame...
                if veryDifferent.status == STARTED:
                    # update params
                    pass
                
                # *verySimilar* updates
                
                # if verySimilar is starting this frame...
                if verySimilar.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    verySimilar.frameNStart = frameN  # exact frame index
                    verySimilar.tStart = t  # local t and not account for scr refresh
                    verySimilar.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(verySimilar, 'tStartRefresh')  # time at next scr refresh
                    # add timestamp to datafile
                    thisExp.timestampOnFlip(win, 'verySimilar.started')
                    # update status
                    verySimilar.status = STARTED
                    verySimilar.setAutoDraw(True)
                
                # if verySimilar is active this frame...
                if verySimilar.status == STARTED:
                    # update params
                    pass
                
                # check for quit (typically the Esc key)
                if defaultKeyboard.getKeys(keyList=["escape"]):
                    thisExp.status = FINISHED
                if thisExp.status == FINISHED or endExpNow:
                    endExperiment(thisExp, inputs=inputs, win=win)
                    return
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    routineForceEnded = True
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in stimComponents:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # --- Ending Routine "stim" ---
            for thisComponent in stimComponents:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            thisExp.addData('stim.stopped', globalClock.getTime())
            trials.addData('slider.response', slider.getRating())
            trials.addData('slider.rt', slider.getRT())
            # the Routine "stim" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            thisExp.nextEntry()
            
            if thisSession is not None:
                # if running in a Session with a Liaison client, send data up to now
                thisSession.sendExperimentData()
        # completed 1.0 repeats of 'trials'
        
    # completed 1.0 repeats of 'task'
    
    
    # --- Prepare to start Routine "end" ---
    continueRoutine = True
    # update component parameters for each repeat
    thisExp.addData('end.started', globalClock.getTime())
    resp_end.keys = []
    resp_end.rt = []
    _resp_end_allKeys = []
    # keep track of which components have finished
    endComponents = [text_end, resp_end]
    for thisComponent in endComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    frameN = -1
    
    # --- Run Routine "end" ---
    routineForceEnded = not continueRoutine
    while continueRoutine:
        # get current time
        t = routineTimer.getTime()
        tThisFlip = win.getFutureFlipTime(clock=routineTimer)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *text_end* updates
        
        # if text_end is starting this frame...
        if text_end.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            text_end.frameNStart = frameN  # exact frame index
            text_end.tStart = t  # local t and not account for scr refresh
            text_end.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(text_end, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'text_end.started')
            # update status
            text_end.status = STARTED
            text_end.setAutoDraw(True)
        
        # if text_end is active this frame...
        if text_end.status == STARTED:
            # update params
            pass
        
        # *resp_end* updates
        waitOnFlip = False
        
        # if resp_end is starting this frame...
        if resp_end.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            resp_end.frameNStart = frameN  # exact frame index
            resp_end.tStart = t  # local t and not account for scr refresh
            resp_end.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(resp_end, 'tStartRefresh')  # time at next scr refresh
            # add timestamp to datafile
            thisExp.timestampOnFlip(win, 'resp_end.started')
            # update status
            resp_end.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(resp_end.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(resp_end.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if resp_end.status == STARTED and not waitOnFlip:
            theseKeys = resp_end.getKeys(keyList=['space'], ignoreKeys=["escape"], waitRelease=False)
            _resp_end_allKeys.extend(theseKeys)
            if len(_resp_end_allKeys):
                resp_end.keys = _resp_end_allKeys[-1].name  # just the last key pressed
                resp_end.rt = _resp_end_allKeys[-1].rt
                resp_end.duration = _resp_end_allKeys[-1].duration
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if defaultKeyboard.getKeys(keyList=["escape"]):
            thisExp.status = FINISHED
        if thisExp.status == FINISHED or endExpNow:
            endExperiment(thisExp, inputs=inputs, win=win)
            return
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            routineForceEnded = True
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in endComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # --- Ending Routine "end" ---
    for thisComponent in endComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    thisExp.addData('end.stopped', globalClock.getTime())
    # check responses
    if resp_end.keys in ['', [], None]:  # No response was made
        resp_end.keys = None
    thisExp.addData('resp_end.keys',resp_end.keys)
    if resp_end.keys != None:  # we had a response
        thisExp.addData('resp_end.rt', resp_end.rt)
        thisExp.addData('resp_end.duration', resp_end.duration)
    thisExp.nextEntry()
    # the Routine "end" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # mark experiment as finished
    endExperiment(thisExp, win=win, inputs=inputs)


def saveData(thisExp):
    """
    Save data from this experiment
    
    Parameters
    ==========
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    """
    filename = thisExp.dataFileName
    # these shouldn't be strictly necessary (should auto-save)
    thisExp.saveAsWideText(filename + '.csv', delim='auto')
    thisExp.saveAsPickle(filename)


def endExperiment(thisExp, inputs=None, win=None):
    """
    End this experiment, performing final shut down operations.
    
    This function does NOT close the window or end the Python process - use `quit` for this.
    
    Parameters
    ==========
    thisExp : psychopy.data.ExperimentHandler
        Handler object for this experiment, contains the data to save and information about 
        where to save it to.
    inputs : dict
        Dictionary of input devices by name.
    win : psychopy.visual.Window
        Window for this experiment.
    """
    if win is not None:
        # remove autodraw from all current components
        win.clearAutoDraw()
        # Flip one final time so any remaining win.callOnFlip() 
        # and win.timeOnFlip() tasks get executed
        win.flip()
    # mark experiment handler as finished
    thisExp.status = FINISHED
    # shut down eyetracker, if there is one
    if inputs is not None:
        if 'eyetracker' in inputs and inputs['eyetracker'] is not None:
            inputs['eyetracker'].setConnectionState(False)
    logging.flush()


def quit(thisExp, win=None, inputs=None, thisSession=None):
    """
    Fully quit, closing the window and ending the Python process.
    
    Parameters
    ==========
    win : psychopy.visual.Window
        Window to close.
    inputs : dict
        Dictionary of input devices by name.
    thisSession : psychopy.session.Session or None
        Handle of the Session object this experiment is being run from, if any.
    """
    thisExp.abort()  # or data files will save again on exit
    # make sure everything is closed down
    if win is not None:
        # Flip one final time so any remaining win.callOnFlip() 
        # and win.timeOnFlip() tasks get executed before quitting
        win.flip()
        win.close()
    if inputs is not None:
        if 'eyetracker' in inputs and inputs['eyetracker'] is not None:
            inputs['eyetracker'].setConnectionState(False)
    logging.flush()
    if thisSession is not None:
        thisSession.stop()
    # terminate Python process
    core.quit()


# if running this experiment as a script...
if __name__ == '__main__':
    # call all functions in order
    expInfo = showExpInfoDlg(expInfo=expInfo)
    thisExp = setupData(expInfo=expInfo)
    logFile = setupLogging(filename=thisExp.dataFileName)
    win = setupWindow(expInfo=expInfo)
    inputs = setupInputs(expInfo=expInfo, thisExp=thisExp, win=win)
    run(
        expInfo=expInfo, 
        thisExp=thisExp, 
        win=win, 
        inputs=inputs
    )
    saveData(thisExp=thisExp)
    quit(thisExp=thisExp, win=win, inputs=inputs)
