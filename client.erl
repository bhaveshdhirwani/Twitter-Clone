-module(client).

-export([start/0, registerUser/0, getUserDetails/0, loginUser/0, homePage/1, tweet/1, searchTweet/0, showHomePage/1, subscribeUser/1, myMentions/1, retweet/1]).

start() ->
    server:start(),
    io:format("Choose an option:~n"),
    io:format("1. Sign In~n"),
    io:format("2. Sign Up~n"),
    {ok, Option} = io:read("Enter a number: "),
    case Option of
        1 -> loginUser();
        2 -> registerUser()
    end.
    
registerUser() ->
    Username = re:replace(io:get_line("Please enter a username: "), "[\r\n]", "", [global, {return, list}]),
    Password = re:replace(io:get_line("Please enter a password: "), "[\r\n]", "", [global, {return, list}]),
    server:registerUser(Username, Password).

loginUser() ->
    Username = re:replace(io:get_line("Please enter your username: "), "[\r\n]", "", [global, {return, list}]),
    Password = re:replace(io:get_line("Please enter your password: "), "[\r\n]", "", [global, {return, list}]),
    server:loginUser(Username, Password).

homePage(Username) ->
    io:format("Choose an option:~n"),
    io:format("1. Tweet~n"),
    io:format("2. Retweet~n"),
    io:format("3. Search~n"),
    io:format("4. Subscribe~n"),
    io:format("5. My Mentions~n"),
    {ok, Option} = io:read("Enter a number: "),
    case Option of
        1 -> tweet(Username);
        2 -> retweet(Username);
        3 -> searchTweet();
        4 -> subscribeUser(Username);
        5 -> myMentions(Username)
    end.

showHomePage(Username) ->
    server:showHomePage(Username).

tweet(Username) ->
    Tweet = re:replace(io:get_line("What's happening? "), "[\r\n]", "", [global, {return, list}]),
    server:tweet(Username, Tweet).

searchTweet() ->
    SearchTerm = re:replace(io:get_line("Search Twitter: "), "[\r\n]", "", [global, {return, list}]),
    server:searchTweet(SearchTerm).

subscribeUser(Username) ->
    Subscribee = re:replace(io:get_line("Please enter the username of the account that you want to subscribe: "), "[\r\n]", "", [global, {return, list}]),
    server:subscribeUser(Username, Subscribee).

getUserDetails() ->
    server:getAllUserData().

myMentions(Username) ->
    server:myMentions(Username).

retweet(Username) ->
    server:retweet(Username).