:- module(_, [], [ciaobld(bundleconfig)]).

:- doc(title, "Configuration rules for Ciao Emacs").
:- doc(author, "Ciao Development Team").

% TODO: prune (ciao_emacs moved)
:- use_module(library(system), [file_exists/1, find_executable/2, get_home/1]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(lists), [append/3]).

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(detcheader), [detect_c_headers/1]).

% (included file)

:- use_module(library(pathnames), [path_concat/3]).

% ---------------------------------------------------------------------------

:- bundle_flag(with_emacs_mode, [
    comment("Enable Emacs-based IDE"),
    details(
      % .....................................................................
      "Set to \"yes\" if you wish to install the Ciao emacs libraries which\n"||
      "implement the Emacs-based IDE (integrated development environment)\n"||
      "(highly recommended).  It should be set to no if emacs is not\n"||
      "installed in the system.  It is safe to leave as \"yes\" otherwise."),
    valid_values(['yes', 'no']),
    %
    default_comment("Emacs detected"),
    rule_default(VerifyEmacs, verify_emacs(VerifyEmacs)),
    %
    interactive
]).

verify_emacs(Value) :-
	( emacs_installed -> Value = yes ; Value = no ).

emacs_installed :- find_emacs(_).

% TODO: it should consider auto_install option!
find_emacs(File) :- find_executable('emacs', File).

% ---------------------------------------------------------------------------

:- bundle_flag(emacs_for_ciao, [
    comment("Emacs version to be used"),
    details(
      % .....................................................................
      "The version of emacs that you wish to use with Ciao. The development\n"||
      "environment will be compiled for use with this version."),
    needed_if(flag(with_emacs_mode(yes))),
    rule_default(DefValue, find_emacs(DefValue)),
    %
    interactive
]).

% ---------------------------------------------------------------------------

:- include(.('dot_emacs.config')).

