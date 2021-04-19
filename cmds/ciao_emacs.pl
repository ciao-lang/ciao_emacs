:- module(ciao_emacs, [main/1], [assertions, fsyntax, doccomments, datafacts]).

%! \title Launcher for Emacs+Ciao mode
%  
%  \author The Ciao Develoment Team
%
%  \module
%
%  This module implements an launcher for Emacs using the Ciao mode.
%  
%  ## Usage
%  
%  Execute with `ciao-emacs &`.

% TODO:
%  - custom macOS emacs gui location (hardwired)
%  - Use daemon names:
%      emacs --daemon=ciao-emacs
%      emacsclient -s ciao-emacs
%    or set the server-name variable before server starts
%  - Use server/daemon names for ciao server too
%  - Also allow port names different than 8000 for ciao-server (add option)
%  - Integrate with library(emacs), make sure that we use the right emacs server
%  - ciao-server may need a separate server/port
%  - Load a splash screen
%  - Integrate with xwidget (capture links, e.g., to load examples)

:- use_module(engine(system_info), [get_os/1]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(process)).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(system), [file_exists/1]).

% ---------------------------------------------------------------------------
%! # Options 

parse_args(Args, Rest) :- !, Rest = Args.

% ---------------------------------------------------------------------------
%! # Configuration

% TODO: This may be expensive. Compute statically instead?
:- use_module(library(bundle/bundle_flags), [current_bundle_flag/2]).

% GUI emacs for macOS is usually not in the path
% TODO: customize or add them to the rules for guessing ciao_emacs:emacs_for_ciao?
macos_emacs_gui := '/opt/homebrew/Cellar/emacs-plus\@27/27.2/Emacs.app/Contents/MacOS/Emacs'.
macos_emacs_gui := '/Applications/Emacs.app/Contents/MacOS/Emacs'.

emacs_path := Path :-
    get_os('DARWIN'),
    Path0 = ~macos_emacs_gui,
    file_exists(Path0),
    !,
    Path = Path0.
emacs_path := Path :-
    current_bundle_flag(ciao_emacs:emacs_for_ciao, Path0),
    !,
    Path = Path0.
emacs_path := _ :-
    throw(error(no_emacs, emacs_path/1)).

emacsmode_elisp_dir := ~bundle_path(ciao_emacs, 'elisp').

% ---------------------------------------------------------------------------
%! # Start (GUI) Emacs 

main(Args) :-
    EmacsBin = ~emacs_path,
    CiaoMode = ~path_concat(~emacsmode_elisp_dir, 'ciao-site-file.el'),
    CiaoEmacs = ~bundle_path(ciao_emacs, 'cmds/ciao_emacs.el'),
    parse_args(Args, Rest),
    process_call(EmacsBin, [
        '-q', 
        '-l', CiaoMode,
        '-l', CiaoEmacs|Rest], [status(ExitCode)]),
    halt(ExitCode). % TODO: better way?
