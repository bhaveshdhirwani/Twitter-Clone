-module(server).

-export([start/0, registerUser/2, getAllUserData/0, loginUser/2, tweet/2, searchTweet/1, showHomePage/1, subscribeUser/2, myMentions/1, retweet/1, displayRetweetOption/4]).

start() ->
    process_flag(trap_exit, true),
    io:format("Server Starting...........~n"),
    database_logic:initDB().

registerUser(Username, Password) ->
    io:format("Creating new user...........~n"),
    IsUserCreated = database_logic:storeUserDetails(Username, Password),
    case IsUserCreated of
        true ->
            io:format("User registered!~n"),
            client:homePage(Username);
        false ->
            io:format("Sorry this username is taken!~n"),
            client:registerUser()
    end.

loginUser(Username, Password) ->
    IsUserLoggedIn = database_logic:loginUser(Username, Password),
    case IsUserLoggedIn of
        true ->
            client:homePage(Username);
        false ->
            client:loginUser()
    end.

tweet(Username, Tweet) ->
    IsTweetSuccessful = database_logic:storeTweet(Username, Tweet),
    if
        IsTweetSuccessful == true ->
            io:format("Tweet Posted!~n")
    end.

searchTweet(SearchTerm) ->
    TweetList = database_logic:searchTweet(),
    lists:foreach(fun({Username, Tweet}) ->
        IsMatch = string:str(Tweet, SearchTerm) > 0,
        if
            IsMatch == true ->
                io:format("~s:~n", [Username]),
                io:format("~s~n~n", [Tweet]);
            true ->
                ok
        end
    end, TweetList).

myMentions(User) ->
    TweetList = database_logic:searchTweet(),
    lists:foreach(fun({Username, Tweet}) ->
        IsMatch = string:str(Tweet, "@"++User) > 0,
        if
            IsMatch == true ->
                io:format("~s:~n", [Username]),
                io:format("~s~n~n", [Tweet]);
            true ->
                ok
        end
    end, TweetList).

showHomePage(Username) ->
    database_logic:showHomePage(Username).

subscribeUser(Username, Subscribee) ->
    IsUserSubscribed = database_logic:subscribeUser(Username, Subscribee),
    if
        IsUserSubscribed == true ->
            io:format("You are now a subscriber of ~p~n", [Subscribee]);
        true ->
            ok
    end.

getAllUserData() ->
    database_logic:getAllUserAccounts().

retweet(Username) ->
    TweetList = database_logic:searchTweet(),
    displayRetweetOption(1, TweetList, TweetList, Username).

displayRetweetOption(_, [], TweetList, Username) ->
    {ok, Option} = io:read("Choose a tweet to retweet: "),
    {_, Tweet} = lists:nth(Option, TweetList),
    IsRetweetSuccessful = database_logic:storeRetweet(Username, Tweet),
    if
        IsRetweetSuccessful == true ->
            io:format("Retweeted!~n")
    end;
displayRetweetOption(Count, [Head | Tail], TweetList, Username) ->
    {Username, Tweet} = Head,
    io:format("~p. ~s:~n",[Count, Username]),
    io:format("~s~n~n",[Tweet]),
    displayRetweetOption(Count + 1, Tail, TweetList, Username).