-module(database_logic).

-export([initDB/0, storeUserDetails/2, getUserAccounts/1, getAllUserAccounts/0, deleteUser/1, loginUser/2, storeTweet/2, getAllTweets/0, searchTweet/0, showHomePage/1, subscribeUser/2, storeRetweet/2]).

-include_lib("stdlib/include/qlc.hrl").

-record(userDetails, {username, password, subscribers}).
-record(tweetDetails, {username, tweet}).

initDB() ->
    mnesia:create_schema(nodes()),
    mnesia:start(),
    mnesia:change_config(extra_db_nodes,nodes()),
    try
        mnesia:table_info(type, userDetails),
        mnesia:add_table_copy(userDetails, nodes(), disc_copies)
    catch
        exit: _ ->
            mnesia:create_table(userDetails, [{attributes, record_info(fields, userDetails)},
            {type, bag},
            {disc_copies, nodes()}]) 
    end,

    try
        mnesia:table_info(type, tweetDetails),
        mnesia:add_table_copy(tweetDetails, nodes(), disc_copies)
    catch
        exit: _ ->
            mnesia:create_table(tweetDetails, [{attributes, record_info(fields, tweetDetails)},
            {type, bag},
            {disc_copies, nodes()}]) 
    end.

storeUserDetails(Username, Password) ->
    UserMap = maps:from_list(getUserAccounts(Username)),
    IsUserRegistered = maps:is_key(Username, UserMap),
    if 
        IsUserRegistered ->
            false;
        true ->
            AF = fun() ->
                mnesia:write(#userDetails{username = Username, password = Password, subscribers = []})
            end,
            mnesia:transaction(AF),
            true
    end.

getUserAccounts(Username) ->
    AF = fun() ->
        Query = qlc:q([X || X <- mnesia:table(userDetails), X#userDetails.username =:= Username]),
        Results = qlc:e(Query),
        lists:map(fun(Item) -> {Item#userDetails.username, Item#userDetails.password} end, Results)
    end,
    {atomic, User} = mnesia:transaction(AF),
    User.

loginUser(Username, Password) ->
    UserMap = maps:from_list(getUserAccounts(Username)),
    IsUserRegistered = maps:is_key(Username, UserMap),
    if 
        IsUserRegistered ->
            {ok, StoredPassword} = maps:find(Username, UserMap),
            if
                StoredPassword == Password ->
                    io:format("You are now logged in!~n"),
                    true;
                true ->
                    io:format("Wrong Password! Please enter the details again.~n"),
                    false
            end;
        true ->
            io:format("No user with this username found!~n"),
            false
    end.

getAllUserAccounts() ->
    AF = fun() ->
        Query = qlc:q([X || X <- mnesia:table(userDetails)]),
        Results = qlc:e(Query),
        lists:map(fun(Item) -> {Item#userDetails.username, Item#userDetails.password, Item#userDetails.subscribers} end, Results)
    end,
    {atomic, User} = mnesia:transaction(AF),
    User.

storeTweet(Username, Tweet) ->
    AF = fun() ->
        mnesia:write(#tweetDetails{username = Username, tweet = "Tweet: " ++ Tweet})
    end,
    mnesia:transaction(AF),
    true.

getAllTweets() ->
    AF = fun() ->
        Query = qlc:q([X || X <- mnesia:table(tweetDetails)]),
        Results = qlc:e(Query),
        lists:map(fun(Item) -> {Item#tweetDetails.username, Item#tweetDetails.tweet} end, Results)
    end,
    {atomic, Tweet} = mnesia:transaction(AF),
    Tweet.

searchTweet() ->
    AF = fun() ->
        Query = qlc:q([X || X <- mnesia:table(tweetDetails)]),
        Results = qlc:e(Query),
        lists:map(fun(Item) -> {Item#tweetDetails.username, Item#tweetDetails.tweet} end, Results)
    end,
    {atomic, Tweet} = mnesia:transaction(AF),
    Tweet.

showHomePage(Username) ->
    Username.

subscribeUser(Username, Subscribee) ->
    AF = fun() ->
        Query = qlc:q([X || X <- mnesia:table(userDetails), X#userDetails.username =:= Subscribee]),
        Results = qlc:e(Query),
        lists:map(fun(Item) -> {Item#userDetails.username, Item#userDetails.subscribers} end, Results)
    end,
    {atomic, User} = mnesia:transaction(AF),
    if
        User == [] ->
            io:format("No such username found!~n"),
            false;
        true ->
            UserMap = maps:from_list(User),
            SubscriberList = maps:get(Subscribee, UserMap),
            IsAlreadyFollower = lists:member(Username, SubscriberList),
            if
                IsAlreadyFollower == true ->
                    io:format("You are already a follower of this account!~n"),
                    false;
                true ->
                    NewList = [SubscriberList | Username],
                    [U] = mnesia:wread({userDetails, Subscribee}),
                    mnesia:write(U#userDetails{subscribers=NewList}),
                    true
            end
    end.

deleteUser(Username) ->
    F = fun() -> 
        mnesia:delete({userDetails, Username})
    end,
    mnesia:transaction(F).

storeRetweet(Username, Tweet) ->
    AF = fun() ->

        mnesia:write(#tweetDetails{username = Username, tweet = "Retweet: " ++ string:sub_string(Tweet,8)})
    end,
    mnesia:transaction(AF),
    true.