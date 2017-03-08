:- module(_, [], [ciaobld(bundlehooks), dcg]).

:- doc(title,  "Bundle Hooks for Ciao Emacs").

% ===========================================================================
:- doc(section, "Configuration rules").

:- use_module(library(system), [find_executable/2]).

:- bundle_flag(enabled, [
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
    rule_default(HasEmacs, has_emacs(HasEmacs)),
    %
    interactive
]).

has_emacs(Value) :-
	( emacs_installed -> Value = yes ; Value = no ).

emacs_installed :- find_emacs(_).

% TODO: it should consider auto_install option!
find_emacs(File) :- find_executable('emacs', File).

:- bundle_flag(emacs_for_ciao, [
    comment("Emacs version to be used"),
    details(
      % .....................................................................
      "The version of emacs that you wish to use with Ciao. The development\n"||
      "environment will be compiled for use with this version."),
    needed_if(flag(enabled(yes))),
    rule_default(DefValue, find_emacs(DefValue)),
    %
    interactive
]).

% ===========================================================================
:- doc(section, "Build rules").

:- use_module(library(llists), [flatten/2, append/2]).
:- use_module(library(pathnames), [path_concat/3]).
:- use_module(library(system), [using_windows/0]).
:- use_module(library(system), [touch/1]).
:- use_module(library(system_extra), [move_if_diff/3]).
:- use_module(library(system_extra),
	[del_file_nofail/1,
	 del_files_nofail/1]).
:- use_module(ciaobld(config_common), [concat_ext/3]).

:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).
:- use_module(library(bundle/bundle_flags), [get_bundle_flag/2]).
:- use_module(library(bundle/bundle_info), [bundle_version/2]).

% NOTE! Be careful if moving library(emacs/emacs_batch) inside this module.
%   There is builder dependency between these hooks and
%   core/library/emacs/emacs_batch.pl!
%      
:- use_module(library(emacs/emacs_batch), [
	emacs_style_path/2,
	emacs_type/1,
        emacs_batch_call/3,
	emacs_batch_byte_compile/3,
	emacs_update_autoloads/3,
        emacs_clean_log/2]).

enabled := ~get_bundle_flag(ciao_emacs:enabled).

emacsmode_elisp_dir := ~bundle_path(ciao_emacs, 'elisp').

% Here is how this all works: 
% 
% - During build and install of Ciao emacs mode
%
%   * The ciao-config.el.skel file is filled with configuration
%     parameters for the installed system to produce ciao-config.el,
%     which is installed in the libraries. The current Ciao version is
%     also included in ciao-config.el at this time.
%
%   * All .el files in the libraries are byte-compiled.
%    
% - Generating the documentation ('ciao build_docs'):
%
%   * CiaoMode.lpdoc is generated from ciao-documentation.el using
%     emacs (contains and extracts the documentation for all the elisp
%     functions). CiaoMode.pl is included as a chapter in the Ciao
%     manual.

% TODO: define third-party plugin for emacs code (so that we can add extensions to the emacs mode easily?)
% TODO: define one .el per bundle or collect all, during dot_emacs build/installation
%       That may solve @bug{lpdoclibdir_emacs_mode}

'$builder_hook'(item_nested(emacs_mode)).
'$builder_hook'(emacs_mode:assets('elisp/icons')) :- % (for installation)
	enabled(yes).
'$builder_hook'(emacs_mode:assets('elisp', ~emacs_mode_files)) :- % (for installation)
	enabled(yes).
%
'$builder_hook'(emacs_mode:prepare_build_docs) :-
	( enabled(yes) -> prepare_build_docs_emacs_mode
	; true
	).
'$builder_hook'(emacs_mode:build_bin) :-
	( enabled(yes) -> build_emacs_mode
	; true
	).
'$builder_hook'(emacs_mode:build_docs) :- !.
'$builder_hook'(emacs_mode:clean_bin) :- clean_emacs_mode.

build_emacs_mode :-
	% TODO: Use better log names (append them?)
	% First, generate 'ciao-config.el' from 'ciao-config.el.skel'
	generate_emacs_config,
	% Generate autoloads automatically with 'batch-update-autoloads'
	Dir = ~emacsmode_elisp_dir,
	Init = ~path_concat(Dir, 'ciao-site-file.el'),
	emacs_update_autoloads(Dir, 'emacs_mode3', Init),
	% Compile to elisp bytecode the .el files
        EL = ~ciao_mode_el_files,
	emacs_batch_byte_compile(Dir, 'emacs_mode', EL).

% TODO: make it relocatable (from the elisp side)
get_dir_elisp(Dir, EmacsDir) :-
	% TODO: missing escape of Dir2
	Dir2 = ~emacs_style_path(Dir),
	flatten(["\"", ~atom_codes(Dir2), "\""], EmacsDir).

% Path to a bundle command binary, as elisp expression
% TODO: make it relocatable (from the elisp side)
cmdpath_elisp(_Bundle, Cmd, Kind, Expr) :-
	( Kind = plexe -> K = plexe
	; Kind = script -> K = ext(~script_extension)
	),
	CmdName = ~concat_ext(K, Cmd),
	Expr = ~flatten(["(concat ciao-bin-dir \"/", ~atom_codes(CmdName), "\")"]).

% TODO: Why .bat? (only for 'ciaocl' at this moment)
script_extension('.bat') :- ( using_windows ; emacs_type('Win32') ), !.
script_extension('').

:- use_module(ciaobld(manifest_compiler), [
    ensure_load_manifest/1,
    bundle_manual_base/2
]).

% Enumerate all manuals of Bundle
get_bundle_manual_base_elisp(Bundle, Name):-
	ensure_load_manifest(Bundle),
	bundle_manual_base(Bundle, Name).

:- use_module(engine(internals), ['$bundle_id'/1]).

% Manuals from all bundles
all_manuals(Bases) :-
	Bases = ~findall(B, ('$bundle_id'(Bundle), get_bundle_manual_base_elisp(Bundle, B))).

% ---------------------------------------------------------------------------
% Generate ciao-config.el (from ciao-config.el.skel)

% TODO: remove or reduced as much as possible (emacs-mode should talk
% with Ciao to query this information)

:- use_module(library(text_template), [eval_template_file/3]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(ciaobld(install_aux),
	[final_ciao_root/1, final_bundle_path/3, final_builddir_path/3]).

generate_emacs_config :-
	In = ~path_concat(~emacsmode_elisp_dir, 'ciao-config.el.skel'),
	Out = ~path_concat(~emacsmode_elisp_dir, 'ciao-config.el'),
	% manual base names
	all_manuals(Bases),
	elisp_string_list(Bases, BasesStr, []),
	%
	BundleDirCore = ~final_bundle_path(core, '.'),
	BundleDirCiaoEmacs = ~final_bundle_path(ciao_emacs, '.'),
	( '$bundle_id'(lpdoc) ->
	    BundleDirLPDoc = ~final_bundle_path(lpdoc, '.')
	; BundleDirLPDoc = BundleDirCore % TODO: incorrect
	),
	BuildDirBin = ~final_builddir_path(core, 'bin'), % TODO: 'core' hardwired
	BuildDirDoc = ~final_builddir_path(core, 'doc'), % TODO: 'core' hardwired
	%
	eval_template_file(In, [
	    % Emacs type (for ciao mode)
            'CIAO_EMACS_TYPE' = ~emacs_type,
	    %
            'CIAO_VERSION' = ~bundle_version(ciao),
	    % Paths
	    'CIAOROOT' = ~final_ciao_root,
	    'BUILDDIRBIN' = ~get_dir_elisp(BuildDirBin),
	    'BUILDDIRDOC' = ~get_dir_elisp(BuildDirDoc),
	    'BUNDLEDIR_CIAO_EMACS' = ~get_dir_elisp(BundleDirCiaoEmacs),
	    'BUNDLEDIR_LPDOC' = ~get_dir_elisp(BundleDirLPDoc),
	    % Manual bases (for ciao-help.el)
            'MANUAL_BASES' = BasesStr,
	    % Command binaries
	    'CIAOSHELL' = ~cmdpath_elisp(core, 'ciao', script),
	    'CIAOPPSHELL' = ~cmdpath_elisp(ciaopp, 'ciaopp', plexe),
	    'LPDOCEXEC' = ~cmdpath_elisp(lpdoc, 'lpdoc', plexe),
	    'CIAOFMTEXEC' = ~cmdpath_elisp(ciaofmt, 'ciaofmt', plexe)
        ], Out).

elisp_string_list(Xs) --> "(", elisp_string_list_(Xs), ")".

elisp_string_list_([X]) --> !,
	% TODO: missing escape of X
	"\"", emit_atom(X), "\"".
elisp_string_list_([X|Xs]) -->
	% TODO: missing escape of X
	"\"", emit_atom(X), "\"", " ", elisp_string_list_(Xs).
elisp_string_list_([]) --> [].

% ---------------------------------------------------------------------------

% Generation of LPdoc documentation source for emacs-mode based on
% `ciao-documentation.el`. Update `CiaoMode.pl` timestamp if
% `CiaoMode.lpdoc` has changed.
prepare_build_docs_emacs_mode :-
	EmacsModeDir = ~emacsmode_elisp_dir,
	emacs_batch_call(EmacsModeDir, 'emacs_mode2', % TODO: right log name?
	  ['--eval', '(setq load-path (cons "." load-path))',
	   '-l', 'ciao-documentation.el',
	   '-f', 'ciao-mode-documentation']),
	%
	( move_if_diff(~path_concat(EmacsModeDir, 'CiaoMode.new.lpdoc'),
	               ~path_concat(EmacsModeDir, 'CiaoMode.lpdoc'), new) ->
	    touch(~path_concat(EmacsModeDir, 'CiaoMode.pl'))
	; true
	).

%-----------------------------------------------------------------------------

emacs_mode_files :=
    ~append([
      ['ciao-site-file.el'],
      ~ciao_mode_el_files,
      ~ciao_mode_elc_files
    ]).

% TODO: Remember to 'update builder/src/win32/Ciao.iss.skel'
%       if the files here are modified. Ideally, that file should not
%       contain any hardwired list of files.

ciao_mode_lisp_files := [
	'word-help',
	'ciao-help',
	'ciao-faces',
	'ciao-syntax',
	'ciao-parsing',
	'ciao-aux',
	'ciao-font-lock',
	'ciao-vc',
	'ciao-scratchpad',
	'ciao-process',
	'ciao-compile',
	'ciao-loading',
	'ciao-testing',
	'ciao-debugger',
	'ciao-lpdoc',
	'ciao-ciaopp',
	'java-ciaopp',
	'ciao-builder',
	'ciao-optim-comp',
	'ciao-widgets',
	'ciao-common',
	'ciao-config',
	'ciao-splash',
	'ciao-bindings',
	'ciao'].

ciao_mode_el_files := ~add_suffix(~ciao_mode_lisp_files, '.el').
ciao_mode_elc_files := ~add_suffix(~ciao_mode_lisp_files, '.elc').

add_suffix([],     _Suffix, []).
add_suffix([L|Ls], Suffix,  [R|Rs]) :-
	atom_concat(L, Suffix, R),
	add_suffix(Ls, Suffix, Rs).

% ----------------------------------------------------------------------------

:- use_module(library(glob), [glob/3]).
:- use_module(ciaobld(ciaoc_aux), [clean_tree/1]).

clean_emacs_mode :-
	EmacsModeDir = ~emacsmode_elisp_dir,
	clean_tree(EmacsModeDir),
	% TODO: necessary? repeated?
	del_file_nofail(~path_concat(EmacsModeDir, 'ciao-site-file.el')),
	% clean log files
	emacs_clean_log(EmacsModeDir, 'emacs_mode'),
	emacs_clean_log(EmacsModeDir, 'emacs_mode2'),
	emacs_clean_log(EmacsModeDir, 'emacs_mode3'),
	%
	% (automatically generated files)
	del_file_nofail(~path_concat(EmacsModeDir, 'CiaoMode.lpdoc')),
	del_file_nofail(~path_concat(EmacsModeDir, 'ciao-config.el')),
	% TODO: Use common implementation, do not specify suffixes by hand
	del_files_nofail(~add_prefix(~glob(EmacsModeDir, '*.itf|*.po|*.asr|*.testout|*.elc'),
	                             ~atom_concat(EmacsModeDir, '/'))).

add_prefix([],     _Preffix, []).
add_prefix([L|Ls], Preffix,  [R|Rs]) :-
	atom_concat(Preffix, L, R),
	add_prefix(Ls, Preffix, Rs).

% ---------------------------------------------------------------------------

'$builder_hook'(item_nested(dot_emacs)).
:- include(.('dot_emacs.hooks')).

