% (included file)

:- doc(section, "Emacs Environment Configuration").

% ---------------------------------------------------------------------------

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [file_exists/1, get_home/1]).

% TODO: Do not customize this
:- bundle_flag(emacsinitfile, [
    comment("Emacs init file"),
    details( % TODO: configurable?
      % .....................................................................
      "Specify the name of the emacs lisp file defining the Ciao mode."),
    needed_if(flag(enabled(yes))),
    rule_set_value(Value, (
      flag(builder:registration_type(SysregType)),
      get_emacs_init_file(SysregType, Value))),
    interactive
]).

get_emacs_init_file(all, '65ciao-mode-init.el') :-
	% Use if it is debian based
	get_os('LINUX'),
	file_exists('/etc/debian_version'),
	!.
get_emacs_init_file(_, 'ciao-mode-init.el').

% TODO: Change name
:- bundle_flag(update_dotemacs, [
    comment("Modify emacs init file"),
    details(
      % .....................................................................
      "Set to \"yes\" if you wish to configure emacs to work with Ciao\n"||
      "(modify emacs initialization file)."),
    valid_values(['yes', 'no']),
    %
    needed_if(flag(enabled(yes))),
    % TODO: This set value means that no question is really asked
    rule_set_value(Value, (
      flag(builder:registration_type(SysregType)),
      update_dotemacs_(SysregType, Value))),
    rule_default('yes'),
    %
    interactive
]).

% update_dotemacs_(InsType, VerifyEmacs, UpdateEmacs)

update_dotemacs_(all,  no) :- !.
update_dotemacs_(user, yes).

:- bundle_flag(dotemacs, [
    comment("Emacs initialization file"),
    details(
      % .....................................................................
      "Define the emacs initialization file where the Ciao settings will be\n"||
      "added."),
    needed_if(flag(update_dotemacs(yes))),
    rule_default(DefValue, (
      flag(builder:registration_type(SysregType)),
      get_dotemacs(SysregType, DefValue))),
    %
    interactive
]).

get_dotemacs(user) := ~path_concat(~get_home, '.emacs').

:- bundle_flag(emacs_site_start, [
    comment("Emacs site start"),
    details(
      % .....................................................................
      "Specify in what file/directory you want to insert/copy the Ciao Emacs\n"||
      "Mode initialization code."),
    needed_if(flag(enabled(yes))),
    rule_default(Value, (
      flag(builder:registration_type(SysregType)),
      flag(builder:instype(InsType)),
      get_emacs_site_start(emacs, SysregType, InsType, Value))),
    %
    interactive([advanced])
]).

:- use_module(ciaobld(install_aux), [inst_bundle_path/3]).

get_emacs_site_start(EmacsKind, all, global, Value) :-
	emacs_site_start_(EmacsKind, Value),
	!.
get_emacs_site_start(_, _, InsType, Value) :-
	% TODO: probably incorrect
	( InsType = local -> Value = ~bundle_path(ciao_emacs, '.')
	; InsType = global -> Value = ~inst_bundle_path(ciao_emacs, '.')
	; fail
	).

emacs_site_start_(EmacsKind, SiteStart) :-
	possible_emacs_site_start(EmacsKind, SiteStart),
	file_exists(SiteStart),
	!.

% Note: this returns files or directories, as follows:
%  .../site-lisp/site-start.d  ==> must put a script in that directory
%  .../site-lisp               ==> must use/create a site-start.el
possible_emacs_site_start(emacs) :=
	'/Applications/Emacs.app/Contents/Resources/site-lisp/site-start.d'|
	'/Applications/Emacs.app/Contents/Resources/site-lisp'|
	'/usr/share/emacs/site-lisp/site-start.d'|
	'/usr/share/emacs/site-lisp'|
	'/etc/emacs/site-start.d'.

% ---------------------------------------------------------------------------

:- use_module(ciaobld(install_aux), [instype/1, inst_bundle_path/3]).

:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(lists), [append/3]).
:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(system), [mktemp_in_tmp/2]).
:- use_module(library(system_extra), [warn_on_nosuccess/1]).
:- use_module(ciaobld(install_aux),
	[rootprefix/1, instdir_install/1, instdir_uninstall/1]).
:- use_module(ciaobld(register_in_script), [
 	register_in_script/3, unregister_from_script/2]).

emacsinitfile := ~get_bundle_flag(ciao_emacs:emacsinitfile).
update_dotemacs := ~get_bundle_flag(ciao_emacs:update_dotemacs).
dotemacs := ~get_bundle_flag(ciao_emacs:dotemacs).
emacs_site_start := ~get_bundle_flag(ciao_emacs:emacs_site_start).

'$builder_hook'(dot_emacs:register) :-
	( enabled(yes) ->
	    ( emacs_init_file(InitFile) ->
	        % Register in some .emacs
	        warn_on_nosuccess(register_in_script(InitFile, ";", ~emacs_load_script))
	    ; has_site_start_d(SiteStartD) ->
	        % Register at emacs site start dir
	        mktemp_in_tmp('emacsloaderXXXXXX', Loader),
		string_to_file(~emacs_load_script, Loader),
		InitFile = ~path_concat(SiteStartD, ~emacsinitfile),
		instdir_install(dir(SiteStartD)),
		instdir_install(file(Loader, InitFile)),
		del_file_nofail(Loader)
	    ; % Do not register
	      true
	    )
	; true
	).
'$builder_hook'(dot_emacs:unregister) :-
	( enabled(yes) ->
	    ( emacs_init_file(InitFile) ->
	        warn_on_nosuccess(unregister_from_script(InitFile, ";"))
	    ; has_site_start_d(SiteStartD) ->
	        InitFile = ~path_concat(SiteStartD, ~emacsinitfile),
		instdir_uninstall(file(InitFile))
            ; % Do not unregister
	      true
	    )
	; true
	).

has_site_start_d(SiteStartD) :-
	SiteStartD = ~emacs_site_start,
	is_site_start_d(SiteStartD).

emacs_load_script(S) :-
	Lib = ~final_ciaolibemacs,
	Lib2 = ~emacs_style_path(Lib),
	emacs_load_script_(Lib2, S, []).

emacs_load_script_(Lib) -->
	"(if (file-exists-p \"", emit_atom(Lib), "\")\n"||
	"  (load-file \"", emit_atom(Lib), "\"))\n".

% (emit an atom codes in a DCG)
emit_atom(X, S, S0) :-
	atom_codes(X, Codes),
	append(Codes, S0, S).

% Final path for the 'ciao-site-file.el' file
final_ciaolibemacs := ~final_bundle_path(ciao_emacs, 'elisp/ciao-site-file.el').

% Obtain the appropriate configuration file for this system or
% installation (.emacs or site-start.el). This predicate fails if no
% change is required, because the mode is installed through the
% site-start.d/ directory (Debian only?), or because the Ciao Emacs
% Mode is disabled.
emacs_init_file := InitFile :-
        ( % Local installation, register in your .emacs file
	  instype(local), update_dotemacs(yes) ->
	    InitFile = ~dotemacs
	; % Register in the site-start.el file:
	  %  - if the site-start.d directory was not found
	  %  - and, we are not using rootprefix (--destdir=DIR in
	  %    install)
	  instype(global),
	  \+ (rootprefix(Prefix), \+ Prefix = ''),
	  Dir = ~emacs_site_start,
	  \+ is_site_start_d(Dir) ->
	    InitFile = ~path_concat(Dir, 'site-start.el')
	; % No init file has to be modified
	  % (see ciao_mode_init_desc/1 for site-start.d installation)
	  fail
	).

% Check that Dir is a site-start.d directory
is_site_start_d(Dir) :-
        atom_concat(_, '/site-start.d', Dir).

