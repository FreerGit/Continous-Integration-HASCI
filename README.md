# Continous-Integration-HASCI

Following learning material: https://leanpub.com/simple-haskell-book

## Run the CI server
You need to install stack and alternativly ghci aswell, simply follow https://docs.haskellstack.org/en/stable/install_and_upgrade/ to get stack.

To actually run the server we need to start up the server itself and agents, the server will listen to incoming requests and queue work to the agents.
The system is distibuted so feel free to start as many agents as you wish! 

As an example:
```
stack run -- start-server 
stack run -- start-agent
stack run -- start-agent
```
Run these in seperate terminals, you now have one server that can delegate work to two agents.

Lets queue a job:

```curl -X POST -H "Content-Type: application/json" -d "$(curl -X GET https://api.github.com/repos/freergit/continous-integration-hasci/events)" "http://localhost:9000/webhook/github"```

The above command fetches the most recent pushEvents from this repo, sends it to the server who then queues the job. You may of course give other pushEvents from another repo if you wish. Remember to have a .hasci.yml file in the repo though!


This will send the example github pushEvent located locally under the test folder, it will pull a .hasci.yml file from my repo https://github.com/FreerGit/HASCI-pipeline-test
and build/execute it.

For this to work you obviously need a running docker daemon, to cleanup the images and containers run:

```stack run -- cleanup-docker``` 


You can use any .yml file you wish, by default the server will look for a .hasci.yml file though.

