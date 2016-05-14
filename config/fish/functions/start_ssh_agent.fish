# Adapted from (https://gist.github.com/rsff/9366074)

setenv SSH_ENV $HOME/.ssh/environment

function test_identities
  if ssh-add -l | grep "The agent has no identities" > /dev/null
    ssh-add
    if [ $status -eq 2 ]
      start_ssh_agent
    end
  end
end

function start_ssh_agent
	if [ -n "$SSH_AGENT_PID" ]
		if ps -ef | grep $SSH_AGENT_PID | grep ssh-agent > /dev/null
    	test_identities
		end
	else
		if [ -f $SSH_ENV ]
    	source $SSH_ENV > /dev/null
		end
  	if ps -ef | grep $SSH_AGENT_PID | grep -v grep | grep ssh-agent > /dev/null
      test_identities
  	else
  		echo "Initializing new SSH agent ..."
      ssh-agent -c | sed 's/^echo/#echo/' > $SSH_ENV
  		echo "succeeded"
			chmod 600 $SSH_ENV
		 	source $SSH_ENV > /dev/null
  		ssh-add
		end
	end
end
