:- module(_, [main/1]).

:- use_module(library(pathnames)).
:- use_module(library(system)).
:- use_module(library(lists)).

% ------------------------------------------------------------------------------
% Entry point
% ------------------------------------------------------------------------------

main([]) :- usage, fail.
main(['-h'|_]) :- usage.
main(['--help'|_]) :- usage.
main([File|Types]) :-
    absolute_file_name(File, X),
    find_default(X, Types).

usage :-
    write('Usage: related-file <file> <type>...'),
    nl.

find_default(X, []) :-
    path_splitext(X, _, XExt),
    default_types_for_ext(XExt, Types),
    find_related_files_of_types(X, Types).
find_default(X, Types) :-
    find_related_files_of_types(X, Types).

find_related_files_of_types(_, []).
find_related_files_of_types(X, [Type|Types]) :-
    (find_related_file_of_type(X, Type); true),
    find_related_files_of_types(X, Types).

find_related_file_of_type(X, Type) :-
    related_file(X, Y, Type),
    write(Type),
    write(' '),
    write(Y),
    nl.

% ------------------------------------------------------------------------------
% API
% ------------------------------------------------------------------------------

related_file(X, Y, Type) :-
    path_splitext(X, _, XExt),
    related_file_by_ext(XExt, X, Y, Type),
    file_exists(Y).

% ------------------------------------------------------------------------------
% Utilities
% ------------------------------------------------------------------------------

file_in_directory(Dir, File) :-
    file_exists(Dir),
    directory_files(Dir, Files),
    member(File, Files).

locate_dominating_file(Start, File, Root) :-
    path_concat(Start, File, Path),
    if(file_exists(Path),
       Start = Root,
       if(Start = '/',
          fail,
          (path_split(Start, Parent, _),
           locate_dominating_file(Parent, File, Root)))).

% ------------------------------------------------------------------------------
% Mappings
% ------------------------------------------------------------------------------

default_types_for_ext(Ext, Types) :- js_ext(Ext), js_types(Types).
default_types_for_ext(Ext, Types) :- elixir_ext(Ext), elixir_types(Types).
default_types_for_ext('.el', Types) :- elisp_types(Types).
default_types_for_ext(_, []).

related_file_by_ext(XExt, X, Y, Type) :-
    js_ext(XExt),
    js_related_file(X, Y, Type).

related_file_by_ext('.el', X, Y, Type) :-
    path_split(X, XDir, XBase),
    elisp_related_file(XBase, YBase, Type),
    path_concat(XDir, YBase, Y).

related_file_by_ext(Ext, X, Y, Type) :-
    elixir_ext(Ext),
    elixir_related_file(X, Y, Type).

% ------------------------------------------------------------------------------
% JavaScript
% ------------------------------------------------------------------------------

js_ext(Ext) :- member(Ext, ['.ts', '.tsx', '.js', '.jsx']).

js_types(['impl', 'test', 'stories']).

js_related_file(X, Y, 'impl') :-
    path_split(X, XDir, XBase),
    js_remove_extra_suffix(['.test', '.spec', '.stories'], XBase, YBase),
    js_remove_optional_dir(['__tests__', '__stories__'], XDir, YDir),
    path_concat(YDir, YBase, Y).

js_related_file(X, Y, 'test') :-
    path_split(X, XDir, XBase),
    js_append_optional_dir('__tests__', XDir, YDir),
    js_add_extra_suffix(['.spec', '.test'], XBase, YBase),
    path_concat(YDir, YBase, Y).

js_related_file(X, Y, 'stories') :-
    path_split(X, XDir, XBase),
    js_append_optional_dir('__stories__', XDir, YDir),
    js_add_extra_suffix(['.stories'], XBase, YBase),
    path_concat(YDir, YBase, Y).

js_remove_extra_suffix(ExtraSuffixes, Base, RemovedBase) :-
    path_splitext(Base, NoExt, Ext),
    path_splitext(NoExt, NoExt1, Ext1),
    member(Ext1, ExtraSuffixes),
    atom_concat(NoExt1, Ext, RemovedBase).

js_remove_optional_dir(Dirs, OldDir, NewDir) :-
    path_split(OldDir, OldDir1, SubDir),
    if(member(SubDir, Dirs),
       NewDir = OldDir1,
       NewDir = OldDir).

js_append_optional_dir(SubDir, OldDir, NewDir) :-
    path_concat(OldDir, SubDir, NewDir1),
    member(NewDir, [OldDir, NewDir1]).

js_add_extra_suffix(ExtraSuffixes, OldBase, NewBase) :-
    path_splitext(OldBase, NoExt, Ext),
    member(ExtraSuffix, ExtraSuffixes),
    atom_concat(ExtraSuffix, Ext, NewExt),
    atom_concat(NoExt, NewExt, NewBase).

% ------------------------------------------------------------------------------
% Emacs Lisp
% ------------------------------------------------------------------------------

elisp_types(['impl', 'test']).

elisp_related_file(XBase, YBase, 'test') :-
    path_splitext(XBase, XNoExt, _),
    atom_concat(XNoExt, '-test.el', YBase).

elisp_related_file(XBase, YBase, 'impl') :-
    path_splitext(XBase, XNoExt, _),
    atom_concat(YNoExt, '-test', XNoExt),
    atom_concat(YNoExt, '.el', YBase).

% ------------------------------------------------------------------------------
% Elixir
% ------------------------------------------------------------------------------

elixir_ext(Ext) :- member(Ext, ['.ex', '.exs']).

elixir_types(['lib', 'test']).

elixir_related_file(X, Y, Type) :-
    path_split(X, XDir, XBase),
    locate_dominating_file(XDir, 'mix.exs', MixRoot),
    path_get_relative(MixRoot, XDir, XRelative),
    path_split_list(XRelative, XSegments),
    elixir_dir_pair(XSegments, YSegments, Type),
    elixir_base(XBase, YBase, Type),
    path_concat_list(YSegments, YRelative),
    path_concat(MixRoot, YRelative, YDir),
    path_concat(YDir, YBase, Y).

elixir_dir_pair(['lib'|Rest], ['test'|Rest], 'test').
elixir_dir_pair(['test'|Rest], ['lib'|Rest], 'lib').

elixir_base(XBase, YBase, 'test') :-
    path_splitext(XBase, XNoExt, _),
    atom_concat(XNoExt, '_test.exs', YBase).

elixir_base(XBase, YBase, 'lib') :-
    path_splitext(XBase, XNoExt, _),
    atom_concat(YNoExt, '_test', XNoExt),
    atom_concat(YNoExt, '.ex', YBase).
