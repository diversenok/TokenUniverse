# Token Universe

**Token Universe** is an advanced tool for experimenting and researching **Windows security mechanisms**. It exposes UI for creating, viewing, impersonating, and modifying access tokens, spawning processes, managing Local Security Authority, checking available access to many types of objects, and more. The program can operate and (at least partially) provide valuable functionality under a wide range of privileges, from *LPAC AppContainer* sandbox to SYSTEM with *SeTcbPrivilege* and *SeCreateTokenPrivilege*.

## Downloads

The tool supports Windows 7 and above. See the **[releases](https://github.com/diversenok/TokenUniverse/releases)** page for pre-compiled binaries.

If you encounter bugs and know how to reproduce them, feel free to [open issues](https://github.com/diversenok/TokenUniverse/issues). Additionally, you can download **debug symbols** (TokenUniverse.dbg) from the releases page and place them into the same folder as TokenUniverse.exe, allowing the program to show stack traces on unhandled exceptions.

For instructions on how to compile the project, see [a section below](#compiling).

Key           | Value
------------- | -----
Author        | diversenok
Version       | 0.3
Date          | March 25th, 2023
Compiled with | Embarcadero Delphi 10.4

# Features

## Viewing & Adjusting Tokens

![Information Window](https://user-images.githubusercontent.com/30962924/180661335-bf436a31-5364-4bd3-b353-95c63da8dbc9.PNG)

## Opening & Creating Tokens

![Main Window](https://user-images.githubusercontent.com/30962924/180661344-01852c95-ab71-4dec-b987-08009109f91e.PNG)

### Example: Logon 

![Logon Window](https://user-images.githubusercontent.com/30962924/227736765-6d25b466-df8e-4606-ad17-de8864c50652.png)

### Example: Creation

![Creation Window](https://user-images.githubusercontent.com/30962924/180661357-b314c66f-d142-4ea1-90fe-f2c37f07e45d.PNG)

## Spawning Processes

![Run Window](https://user-images.githubusercontent.com/30962924/180661363-c4210fd9-ef39-4d9d-a8ed-844a5f7bab39.PNG)

## Checking Access

![Access Check Window](https://user-images.githubusercontent.com/30962924/227734301-782086c6-aad0-4e67-ac83-32e574dd3ed1.png)

## Other

![Other](https://user-images.githubusercontent.com/30962924/180661365-e2a5c35a-3024-4812-b728-e3e364f2dd2f.PNG)

## Feature list

There are a lot of already implemented features, but there are also many more to go. Here is the overview of both:

### Token-related functionality

#### Obtaining tokens
 - [x] Opening process/thread tokens
 - [ ] Opening all accessible process/thread tokens
 - [x] Opening tokens via direct impersonation
 - [x] Querying terminal session token
 - [x] Logging in users with credentials
 - [x] Logging in users without credentials (S4U logon)
 - [x] Logging into virtual accounts
 - [x] Adding arbitrary group membership while logging users in
 - [x] Duplicating tokens
 - [x] Duplicating handles
 - [x] Searching for opened handles
 - [x] Opening linked tokens
 - [x] Creating restricted tokens
 - [x] Creating restricted tokens via Safer API
 - [ ] Creating LowBox (AppContainer) tokens
 - [x] Creating tokens via NtCreareToken
 - [x] Creating anonymous token
 - [x] Creating anonymous token with Everyone membership
 - [ ] Impersonating logon session token via pipes
 - [ ] Opening clipboard token
 - [ ] Impersonating BITS

#### Viewing & editing
 - [x] Groups (enable/disable)
 - [x] Privileges (enable/disable/remove)
 - [x] Session ID
 - [x] Integrity level (lower/raise)
 - [x] UIAccess, mandatory policy
 - [x] Virtualization (enable/disable & allow/disallow)
 - [x] Owner and primary group
 - [x] Originating logon session
 - [ ] Default DACL
 - [ ] Security descriptor
 - [ ] Security attributes
 - [x] Audit overrides
 - [ ] Handle flags (inherit, protect)
 - [x] SID info
 - [ ] Privilege info

#### Just Viewing
 - [x] User, restricting SIDs
 - [x] Statistics, source, flags
 - [x] Extended flags (TOKEN_\*)
 - [x] App container SID
 - [ ] Verbose terminal session information
 - [x] Elevation type
 - [x] Logon session information
 - [ ] Package identity information
 - [ ] Trust level
 - [ ] Capabilities
 - [ ] Claims
 - [ ] AppModel policy
 - [x] Object and handle information (access, attributes, references)
 - [x] Object creator (PID)
 - [x] List of processes that have handles to this token
 - [ ] Creation and last modification times

#### Operations
 - [x] Impersonation
 - [x] Safe impersonation
 - [ ] Direct impersonation
 - [x] Assigning primary tokens
 - [x] Sending handles to another process
 - [x] Spawning processes with tokens
 - [ ] Share with another instance of TokenUniverse
 - [ ] Comparing tokens
 - [ ] Linking logon sessions to create UAC-friendly tokens

#### AppContainers and Packages
 - [x] Viewing AppContainer information
 - [ ] Listing AppContainer profiles per user
 - [x] Listing child AppContainers
 - [ ] Creating/deleting AppContainers 
 - [ ] Viewing package information
 - [ ] Listing packages

### Local Security Authority
 - [x] Global audit settings
 - [x] Per-user audit settings
 - [x] Privilege assignment
 - [x] Logon rights assignment
 - [ ] Virtual account creation
 - [ ] SID tree
 - [ ] Security
 - [ ] Enumerating accounts with right/privilege
 - [ ] Source of rights in the token
 - [ ] Quotas
 
### Security Account Manager
 - [ ] Domain information
 - [ ] Group information
 - [ ] Alias information
 - [ ] User information
 - [x] Enumerate domain groups/aliases/users
 - [ ] Enumerate group members
 - [ ] Enumerate alias members
 - [ ] Manage group members
 - [ ] Manage alias members
 - [ ] Create groups
 - [ ] Create aliases
 - [ ] Create users
 - [ ] Sam object tree
 - [ ] Security

### Process creation

#### Methods
 - [x] CreateProcessAsUser
 - [x] CreateProcessWithToken
 - [x] CreateProcessWithLogon (credentials)
 - [x] CreateProcess via code injection (no token)
 - [x] RtlCreateUserProcess
 - [x] RtlCreateUserProcessEx
 - [x] NtCreateUserProcess
 - [x] NtCreateProcessEx
 - [x] ShellExecuteEx (no token)
 - [x] ShellExecute via IShellDispatch2 (no token)
 - [x] WdcRunTaskAsInteractiveUser (no token)
 - [x] WMI
 - [ ] IDesktopAppXActivator (packaged)
 
#### Parameters
 - [x] Current directory
 - [x] Desktop
 - [x] Window show mode
 - [x] Flags (inherit handles, create suspended, breakaway from job, ...)
 - [x] Force job breakaway
 - [ ] Environmental variables
 - [x] Parent process override
 - [ ] Mitigation policies
 - [x] Child process policy
 - [x] Process protection (PPL/Full/etc.)
 - [x] Custom SxS registration
 - [ ] Job assignment
 - [x] Run as invoker/ignore elevation
 - [x] AppContainer SID
 - [ ] Capabilities
 - [ ] Security descriptor
 - [x] Verify access to desktop/window station

### Process & thread list
 - [x] Hierarchy
 - [x] Icons
 - [ ] Listing processes from Low integrity & AppContainer
 - [x] Suspend/resume support
 - [ ] Customizable columns
 - [ ] Highlighting
 - [ ] Security

### Attack Surface Analysis
 - [x] Checking access to NT namespace objects
 - [x] Namespace object name suggestions
 - [x] Checking access to processes/threads/tokens
 - [x] Checking access to LSA and SAM accounts
 - [ ] Checking access to window stations and desktops
 - [ ] Checking access to services and SCM
 - [ ] Checking access to file shares
 - [ ] Checking access to kernel transactions
 - [ ] Checking access to objects in private namespaces
 - [ ] Enumerating accessible resources

### Other
 - [x] Restarting as admin
 - [x] Restarting as SYSTEM
 - [x] Restarting as SYSTEM with SeCreateToken
 - [x] Customizable columns
 - [ ] Settings
 - [ ] Sharing settings across users
 - [ ] Graphical hash icons
 - [x] Auto-detecting inherited handles
 - [x] SID suggestions
 - [x] Detailed error status information
 - [ ] DLL mode

# Compiling

To compile the tool, you can use the free [Community Edition of Embarcadero Delphi](https://www.embarcadero.com/products/delphi/starter). After installing it, the steps are the following:

1. Clone the project **and its submodule dependencies** using `git clone --recurse-submodules`. Alternatively, you can use `git submodule update --init` after cloning the repository.
2. Make sure there are files under the `NtUtilsUI` directory, otherwise, you didn't clone the submodules.
3. Install `VirtualTree for VCL` using the IDE menu `Tools` -> `GetIt Package Manager`.    
![VirtualTree](https://user-images.githubusercontent.com/30962924/180660667-43aa9113-ccc6-4548-8a94-9f81ed84e8eb.png)
4. Open `NtUtilsUI\Components\VirtualTreesExtension.dproj` in the IDE and click `Install` on the project to register it as a design-time package.    
![Install](https://user-images.githubusercontent.com/30962924/180660721-50fe47dc-039d-40cf-a190-c99f91ac0e2d.png)
5. Similarly, open `NtUtilsUI\VclEx\VclExtension.dproj` and click `Install`.
6. Now you can open and build the main `TokenUniverse.dproj` project.

Additionally, if you also want to generate debug symbols during compilation, you'll need **map2dbg** - a tool that converts `*.map` files generated by Delphi into `*.dbg` files that dbghelp.dll can understand. The project is already configured for generating `*.map` files and using a post-build event, so you can download map2dbg from an answer to this [Stack Overflow question](https://stackoverflow.com/questions/9422703) and place it somewhere where the Delphi compiler can find and invoke it.
