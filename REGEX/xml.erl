%% main process keeping track of progress

-module(xml2).
-export([main/1]).
-include_lib("xmerl/include/xmerl.hrl").

parseAll(D) ->
    % find all RSS files underneath D
    FL = filelib:fold_files(D, ".+.rss$", true, fun(F, L) -> [F|L] end, []),
    [ parse(F) || F <- FL ].
 
parse(FName) ->
    % parses a single RSS file
    {R,_} = xmerl_scan:file(FName),
    % extract episode titles, publication dates and MP3 URLs
    L = lists:reverse(extract(R, [])),
    % print channel title and data for first two episodes
    io:format("~n>> ~p~n", [element(1,lists:split(3,L))]),
    L.
 
  % handle 'xmlElement' tags
  extract(R, L) when is_record(R, xmlElement) ->
      case R#xmlElement.name of
          enclosure ->
              if element(1, hd(R#xmlElement.parents)) == item ->
                      FFunc = fun(X) -> X#xmlAttribute.name == url end,
                      U = hd(lists:filter(FFunc, R#xmlElement.attributes)),
                      [ {url, U#xmlAttribute.value} | L ];
                  true -> L
              end;
          channel ->
              lists:foldl(fun extract/2, L, R#xmlElement.content);
          item ->
              ItemData = lists:foldl(fun extract/2, [], R#xmlElement.content),
              [ ItemData | L ];
          _ -> % for any other XML elements, simply iterate over children
              lists:foldl(fun extract/2, L, R#xmlElement.content)
      end;
  
  extract(#xmlText{parents=[{title,_},{channel,2},_], value=V}, L) ->
      [{channel, V}|L]; % extract channel/audiocast title
  
  extract(#xmlText{parents=[{title,_},{item,_},_,_], value=V}, L) ->
      [{title, V}|L]; % extract episode title
  
  extract(#xmlText{parents=[{link,_},{item,_},_,_], value=V}, L) ->
      [{link, V}|L]; % extract episode link
  
  extract(#xmlText{parents=[{pubDate,_},{item,_},_,_], value=V}, L) ->
      [{pubDate, V}|L]; % extract episode publication date ('pubDate' tag)
  
  extract(#xmlText{parents=[{'dc:date',_},{item,_},_,_], value=V}, L) ->
      [{pubDate, V}|L]; % extract episode publication date ('dc:date' tag)
  
  extract(#xmlText{}, L) -> L.  % ignore any other text data
  
  % 'main' function (invoked from shell, receives command line arguments)
  main(A) ->
      D = atom_to_list(hd(A)),
      parseAll(D).