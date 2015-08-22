-module(space).
%% accessor functions
-export([boundaries/1,regions/1,side/3,neighbor/3]).
-export([attached/2,attached/3,shell/2,shell/3,half/2,half/3,corner/2,corner/3]).
%% functions for spaces in same universe
-export([take/4,take/5,subspace/3,section/3,superspace/3,equivalent/2]).
%% functions for spaces in different universe
-export([cospace/3]).
%% functions to convert to and from vectors
-export([sample/4,classify/4]).
