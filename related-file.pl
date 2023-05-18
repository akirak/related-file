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
    default_types_by_ext(XExt, Types),
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
    path_split(X, XDir, XBase),
    path_splitext(XBase, XNoExt, XExt),
    if((related_file_1(XDir, XNoExt, XExt, YDir, YBase, Type),
        file_in_directory(YDir, YBase)),
       path_concat(YDir, YBase, Y),
       false).

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

default_types_by_ext(Ext, Types) :- js_ext(Ext), js_types(Types).
default_types_by_ext(Ext, Types) :- elixir_ext(Ext), elixir_types(Types).
default_types_by_ext('.el', Types) :- elisp_types(Types).
default_types_by_ext(_, []).

related_file_1(XDir, XNoExt, XExt, YDir, YBase, Type) :-
    js_ext(XExt),
    js_related_file(XDir, XNoExt, XExt, YDir, YBase, Type).

related_file_1(XDir, XNoExt, '.el', YDir, YBase, Type) :-
    elisp_related_file(XDir, XNoExt, '.el', YDir, YBase, Type).

related_file_1(XDir, XNoExt, XExt, YDir, YBase, Type) :-
    elixir_ext(XExt),
    elixir_related_file(XDir, XNoExt, XExt, YDir, YBase, Type).

% ------------------------------------------------------------------------------
% JavaScript
% ------------------------------------------------------------------------------

js_ext(Ext) :- member(Ext, ['.ts', '.tsx', '.js', '.jsx']).

js_types(['impl', 'test', 'stories']).

js_related_file(XDir, XNoExt, XExt, YDir, YBase, 'impl') :-
    path_splitext(XNoExt, XNoExt1, XExt1),
    member(XExt1, ['.test', '.spec', '.stories']),
    js_impl_dir(XDir, YDir),
    atom_concat(XNoExt1, XExt, YBase).

js_related_file(XDir, XNoExt, XExt, YDir, YBase, 'test') :-
    js_ext(XExt),
    path_concat(XDir, '__tests__', YDir1),
    member(YDir, [XDir, YDir1]),
    atom_concat('.test', XExt, YExt1),
    atom_concat('.spec', XExt, YExt2),
    member(YExt, [YExt1, YExt2]),
    path_splitext(YBase, XNoExt, YExt).

js_related_file(XDir, XNoExt, XExt, YDir, YBase, 'stories') :-
    js_ext(XExt),
    path_concat(XDir, '__stories__', YDir1),
    member(YDir, [XDir, YDir1]),
    atom_concat('.stories', XExt, YExt),
    path_splitext(YBase, XNoExt, YExt).

js_impl_dir(TestDir, ImplDir) :-
    path_split(TestDir, TestDir1, Subdir),
    member(Subdir, ['__tests__', '__stories__']),
    TestDir1 = ImplDir.

js_impl_dir(TestDir, ImplDir) :-
    TestDir = ImplDir.

% ------------------------------------------------------------------------------
% Emacs Lisp
% ------------------------------------------------------------------------------

elisp_types(['impl', 'test']).

elisp_related_file(XDir, XNoExt, '.el', YDir, YBase, 'test') :-
    XDir = YDir,
    path_splitext(YBase, XNoExt, '-test.el').

elisp_related_file(XDir, XNoExt, '.el', YDir, YBase, 'impl') :-
    XDir = YDir,
    atom_concat(YNoExt, '-test', XNoExt),
    atom_concat(YNoExt, '.el', YBase).

% ------------------------------------------------------------------------------
% Elixir
% ------------------------------------------------------------------------------

elixir_ext(Ext) :- member(Ext, ['.ex', '.exs']).

elixir_types(['lib', 'test']).

elixir_related_file(XDir, XNoExt, _, YDir, YBase, Type) :-
    locate_dominating_file(XDir, 'mix.exs', Root),
    path_get_relative(Root, XDir, XRelative),
    path_split_list(XRelative, XSegments),
    elixir_related_file_1(XSegments, XNoExt, YDir, YBase, Type).

elixir_related_file_1(['lib'|Rest], NonExt, Dir, Base, 'test') :-
    atom_concat(NonExt, '_test.exs', Base),
    list_concat([['test'], Rest], Segments),
    path_concat_list(Segments, Dir).

elixir_related_file_1(['test'|Rest], NonExt, Dir, Base, 'lib') :-
    atom_concat(NewNonExt, '_test', NonExt),
    path_splitext(Base, NewNonExt, '.ex'),
    list_concat([['lib'], Rest], Segments),
    path_concat_list(Segments, Dir).
