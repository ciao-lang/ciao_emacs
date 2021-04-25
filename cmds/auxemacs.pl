:- module(_, [], [fsyntax, doccomments]).

%! \title Auxiliary predicates for emacs interaction
%
%  \module 
%  This module implements some auxiliary predicates for emacs
%  interaction based on `emacsclient`.

:- use_module(library(system)).
:- use_module(library(streams)).
:- use_module(library(process)).
:- use_module(library(emacs)).

:- export(ciao_browse_preprocessor_options/0).
:- pred ciao_browse_preprocessor_options # "Open the CiaoPP options menu".
ciao_browse_preprocessor_options :-
    process_call(path(emacsclient), ['-e', '(set-buffer "*Ciao*")', '-e', '(ciao-browse-preprocessor-options)'], [stdout(null)]).

:- export(pwd/0).
:- pred pwd # "Show the current directory.".
pwd :-
    working_directory(D,D),
    display(D), nl.

:- export(ls/0).
:- pred ls # "Show files in the current directory.".
ls :-
    process_call(path(ls), [], []).

:- export(edit/1).
:- pred edit(F) # "Edit the file `F`.".
edit(F) :- emacs_edit(F).

:- export(find_file/0).
:- pred find_file # "Find a file in the editor (open `dired`).".
find_file :-
    % process_call(path(emacsclient), ['-e', '(set-buffer "*Ciao*")', '-e', '(find-file ".")'], [stdout(null)]).
    process_call(path(emacsclient), ['-e', '(find-file ".")'], [stdout(null)]).

:- export(open_url/1).
open_url(Url) :-
    process_call(path(emacsclient), [
        '-e', '(set-buffer "*Ciao*")',
        '-e', ~atom_concat('(xwidgets-reuse-xwidget-reuse-browse-url "',
                           ~atom_concat(Url, '")'))
    ], [stdout(null)]).

% TODO: create a ciao html buffer abstraction (elisp) based on this, that can be called from lpdoc

% ---------------------------------------------------------------------------

:- export(html_view/0).
:- pred html_view # "Open a HTML-view buffer".
html_view :-
    process_call(path(emacsclient), [
        '-e', '(when (not (xwidget-webkit-current-session)) (xwidget-webkit-new-session "http://localhost:8000"))',
        '-e', '(switch-to-buffer xwidget-webkit-last-session-buffer)'
    ], [stdout(null)]).

:- export(html_refresh/0).
:- pred html_refresh # "Refresh the HTML-view buffer".
html_refresh :-
    process_call(path(emacsclient), [
        '-e', '(xwidget-webkit-reload)'
    ], [stdout(null)]).

:- export(html_replace/1).
:- pred html_replace(X) # "Replace HTML-view with given content".
html_replace(X) :-
    Atm = ~atom_concat('(xwidget-webkit-execute-script (xwidget-webkit-current-session) "document.body.parentElement.innerHTML = \'<html><body><div>',
                       ~atom_concat(X, '</div></body></html>\';\")')),
    process_call(path(emacsclient), [
%        '-e', '(xwidget-webkit-execute-script (xwidget-webkit-current-session) "document.body.parentElement.innerHTML = \'<html><body style=\\"background: black; color: white\\"><div style=\\"font-family: Sometype Mono; font-size: 50px\\">Fine!</div></body></html>\';\")'
        '-e', Atm
    ], [stdout(null)]).

% TODO: injecting arbitrary stuff from ciao via emacsclient may not be very efficient and portable for webapp deployment; use actmod instead?
% (xwidget-webkit-execute-script
% (xwidget-webkit-current-session) "document.body.parentElement.innerHTML = '<html><body style=\"background: black; color: white\"><div style=\"font-family: Sometype Mono; font-size: 50px\">Fine!</div></body></html>';")

% ---------------------------------------------------------------------------

:- export(search/0).
:- pred search # "Open the Ciao search page (HTML)".
search :-
    open_url('https://ciao-lang.org/ciao/build/doc/ciao.html/ciaosearch.html').

:- export(help/0).
:- pred search # "Open the Ciao manual (HTML)".
help :-
    open_url('https://ciao-lang.org/ciao/build/doc/ciao.html/').

% ---------------------------------------------------------------------------

:- export(gitlab_issues/0).
gitlab_issues :-
    open_url('https://gitlab.software.imdea.org/ciao-lang/ciao-devel/-/issues').

:- export(gitlab_mr/0).
gitlab_mr :-
    open_url('https://gitlab.software.imdea.org/ciao-lang/ciao-devel/-/merge_requests').
