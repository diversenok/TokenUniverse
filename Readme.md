# Token Universe

> **Note**:  
> Although the program is under heavy development and there are still plenty of things to do, I am planning to release a test version pretty soon.

**Token Universe** is an advanced tool that provides a wide range of possibilities to research **Windows security mechanisms**. It has a convenient interface for creating, viewing, and modifying access tokens, managing Local Security Authority and Security Account Manager's databases. It allows you to obtain and impersonate different security contexts, manage privileges, auditing settings, and so on.

My goal is to create a useful tool that implements almost everything I know about access tokens and Windows security model in general. *And, also, to learn even more in the process.* I believe that such a program can become a valuable instrument for researchers and those who want to learn more about the security subsystem. You are welcome to suggest any ideas and report bugs.

<details>
  <summary><b>Screenshots</b></summary>
  <img src="https://user-images.githubusercontent.com/30962924/59454197-391ce680-8e1a-11e9-8247-fad8d66b9899.png"/>&nbsp;
  <img src="https://user-images.githubusercontent.com/30962924/59454277-636ea400-8e1a-11e9-8013-1f04804e4c49.png"/>&nbsp;
  <img src="https://user-images.githubusercontent.com/30962924/59454348-8305cc80-8e1a-11e9-8353-214f7a06c617.png"/>&nbsp;
  <img src="https://user-images.githubusercontent.com/30962924/59454367-8b5e0780-8e1a-11e9-8204-5acc392d9bb0.png"/>&nbsp;
  <img src="https://user-images.githubusercontent.com/30962924/50378937-eeca6200-064d-11e9-944a-f168d2bc71c2.png"/>&nbsp;
  <img src="https://user-images.githubusercontent.com/30962924/50378940-06094f80-064e-11e9-8558-472062b290ef.png"/>&nbsp;
</details>

# Feature list

## Token-related functionality

### Obtaining tokens
 - [x] Open process/thread token
 - [x] Open effective thread token (via direct impersonation)
 - [x] Query session token
 - [x] Log in user using explicit credentials
 - [x] Log in user without credentials (S4U logon)
 - [x] Duplicate tokens
 - [x] Duplicate handles
 - [x] Open linked token
 - [x] Filter tokens
 - [ ] Create LowBox tokens
 - [x] Created restricted tokens using Safer API
 - [x] Search for opened handles
 - [x] Create anonymous token
 - [ ] Impersonate logon session token via pipes
 - [ ] Open clipboard token

#### Highly privileged operations
 - [x] Add custom group membership while logging in users (requires *Tcb Privilege*)
 - [x] Create custom token from scratch (requires *Create Token Privilege*)

### Viewing
 - [x] User
 - [x] Statistics, source, flags
 - [x] Extended flags (TOKEN_\*)
 - [x] Restricting SIDs
 - [ ] App container SID and number
 - [ ] Capabilities
 - [ ] Claims
 - [ ] Trust level
 - [x] Logon session type (filtered/elevated/default)
 - [x] Logon session information
 - [ ] Verbose terminal session information
 - [x] Object and handle information (access, attributes, references)
 - [x] Object creator (PID)
 - [x] List of processes that have handles to this object
 - [ ] Creation and last modification times

### Viewing & editing
 - [x] Groups (enable/disable)
 - [x] Privileges (enable/disable/remove)
 - [x] Session
 - [x] Integrity level (lower/raise)
 - [x] UIAccess, mandatory policy
 - [x] Virtualization (enable/disable & allow/disallow)
 - [x] Owner and primary group
 - [x] Originating logon session
 - [ ] Default DACL
 - [ ] Security descriptor
 - [x] Audit overrides
 - [ ] Handle flags (inherit, protect)

### Using
 - [x] Impersonation
 - [x] Safe impersonation
 - [ ] Direct impersonation
 - [x] Assign primary token
 - [x] Send handle to process
 - [x] Create process with token
 - [ ] Share with another instance of TokenUniverse

### Other actions
 - [ ] Compare tokens
 - [ ] Linking logon sessions to create UAC-friendly tokens
 - [ ] Logon session relation map

### AppContainer profiles
 - [x] Viewing AppContainer information
 - [ ] Listing AppContainer profiles per user
 - [x] Listing child AppContainers
 - [ ] Creating/deleting AppContainers

## Local Security Authority
 - [x] Global audit settings
 - [x] Per-user audit settings
 - [x] Privilege assignment
 - [x] Logon rights assignment
 - [ ] Quotas
 - [ ] Security
 - [ ] Enumerate accounts with privilege
 - [ ] Enumerate accounts with right

## Security Account Manager
 - [ ] Domain information
 - [ ] Group information
 - [ ] Alias information
 - [ ] User information
 - [ ] Enumerate domain groups/aliases/users
 - [ ] Enumerate group members
 - [ ] Enumerate alias members
 - [ ] Manage group members
 - [ ] Manage alias members
 - [ ] Create groups
 - [ ] Create aliases
 - [ ] Create users
 - [ ] Sam object tree
 - [ ] Security

## Process creation

### Methods
 - [x] CreateProcessAsUser
 - [x] CreateProcessWithToken
 - [x] WMI
 - [x] RtlCreateUserProcess
 - [x] RtlCreateUserProcessEx
 - [x] NtCreateUserProcess
 - [x] NtCreateProcessEx
 - [x] CreateProcessWithLogon (credentials)
 - [x] ShellExecuteEx (no token)
 - [x] ShellExecute via IShellDispatch2 (no token)
 - [x] CreateProcess via code injection (no token)
 - [x] WdcRunTaskAsInteractiveUser (no token)

### Parameters
 - [x] Current directory
 - [x] Desktop
 - [x] Window show mode
 - [x] Flags (inherit handles, create suspended, breakaway from job, ...)
 - [ ] Environmental variables
 - [x] Parent process override
 - [ ] Mitigation policies
 - [ ] Child process policy
 - [ ] Job assignment
 - [x] Run as invoker compatibility
 - [x] AppContainer SID
 - [ ] Capabilities

### Interface features
 - [ ] Immediate crash notification
 - [ ] Window station and desktop access checks
 - [ ] Debug messages reports

## Process list
 - [x] Hierarchy
 - [x] Icons
 - [ ] Listing processes from Low integrity & AppContainer
 - [ ] Basic actions (resume/suspend, ...)
 - [ ] Customizable columns
 - [ ] Highlighting
 - [ ] Security
 - [ ] Handle table manipulation

## Interface features
 - [x] Restart as SYSTEM
 - [x] Restart as SYSTEM+ (with *Create Token Privilege*)
 - [ ] Customizable columns
 - [ ] Graphical hash icons
 - [x] Auto-detect inherited handles
 - [ ] Our own security editor with arbitrary SIDs and mandatory label modification
 - [ ] Customizable list of suggested SIDs
 - [x] Detailed error status information
 - [ ] Detailed suggestions on errors

## Misc. ideas
 - [?] Logon session creation (requires an authentication package?)
 - [?] ~~Job-based token filtration~~ (unsupported on Vista+)
 - [?] Privilege and audit category description from wsecedit.dll
