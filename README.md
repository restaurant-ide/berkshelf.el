# berkshelf.el
Emacs integration with berkshelf



  berks apply ENVIRONMENT     # Apply version locks from Berksfile.lock to a Chef environment
  berks contingent COOKBOOK   # List all cookbooks that depend on the given cookbook in your Berksfile
  berks cookbook NAME [PATH]  # Create a skeleton for a new cookbook
  berks help [COMMAND]        # Describe available commands or one specific command
  berks info [COOKBOOK]       # Display name, author, copyright, and dependency information about a cookbook
  berks init [PATH]           # Initialize Berkshelf in the given directory
  berks install               # Install the cookbooks specified in the Berksfile
  berks list                  # List cookbooks and their dependencies specified by your Berksfile
  berks outdated [COOKBOOKS]  # List dependencies that have new versions available that satisfy their constraints
  berks package [PATH]        # Vendor and archive the dependencies of a Berksfile
  berks search NAME           # Search the remote source for cookbooks matching the partial name
  berks shelf SUBCOMMAND      # Interact with the cookbook store
  berks show [COOKBOOK]       # Display the path to a cookbook on disk
  berks update [COOKBOOKS]    # Update the cookbooks (and dependencies) specified in the Berksfile
  berks upload [COOKBOOKS]    # Upload the cookbook specified in the Berksfile to the Chef Server
  berks vendor [PATH]         # Vendor the cookbooks specified by the Berksfile into a directory
  berks verify                # Perform a quick validation on the contents of your resolved cookbooks
  berks version               # Display version
  berks viz                   # Visualize the dependency graph

