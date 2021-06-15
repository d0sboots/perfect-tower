// This code treats 0 and 1 as reserved = All and Default
let workspaces = ["All", "Default Workspace"];
let currentWorkspace = workspaces[1];

const workspaceList = document.getElementById("workspaceList");

function workspaceLoad() {
    // Load saved workspaces that do not exist yet
    scripts.forEach((element) => {
        if (!workspaces.includes(element[2])) {
            // console.log("Adding saved workspace - ", element[2]);
            workspaces.push(element[2])
        }
    })

    // Construct select
    workspaceList.innerHTML = "";
    workspaces.forEach((workspaceName) => {
        createWorkspaceElement(workspaceName);
    });

    // Change view
    workspaceChange("All");
}

function createWorkspaceElement(workspaceName) {
    const option = document.createElement("option");
    option.value = workspaceName;
    option.innerHTML = workspaceName;
    workspaceList.appendChild(option);
}

function workspaceNew() {
    const newWorkspaceName = prompt("New Workspace Name:");

    if (!newWorkspaceName || newWorkspaceName.length == 0 || workspaces.includes(newWorkspaceName)) {
        return;
    }

    workspaces.push(newWorkspaceName);
    createWorkspaceElement(newWorkspaceName);
    workspaceChange(newWorkspaceName);
}

function workspaceRename() {
    const currentWorkspace = workspaceList.value;

    // If "All" or "Default"
    if (currentWorkspace === workspaces[0] || currentWorkspace === workspaces[1]) {
        alert(`Cannot rename ${currentWorkspace}`);
        return
    }

    const newWorkspaceName = prompt("New Workspace Name:");

    if (!newWorkspaceName || newWorkspaceName.length == 0) {
        return;
    }

    if (newWorkspaceName === workspaces[0] || newWorkspaceName === workspaces[1]) {
        alert('These names are resevered');
        return
    }

    // Switch all scripts from workspace to new name
    scripts.forEach((script) => {
        if (script[2] === currentWorkspace) {
            script[2] = newWorkspaceName;
        }
    });

    // Filter to remove duplicates
    // Map to rename
    workspaces = workspaces.filter(workspace => workspace !== newWorkspaceName).map(workspace => {
        if (workspace === currentWorkspace) {
            workspace = newWorkspaceName;
        }
        return workspace;
    });

    workspaceLoad();
    workspaceChange(newWorkspaceName);
}

function workspaceDelete() {
    const currentWorkspace = workspaceList.value;

    // If "All" or "Default"
    if (currentWorkspace === workspaces[0] || currentWorkspace === workspaces[1]) {
        alert(`Cannot delete ${currentWorkspace}`);
        return
    }

    if (!confirm(`Are you sure you want to delete workspace ${currentWorkspace}?`)) return;

    // Switch all scripts from workspace to default
    scripts.forEach((script) => {
        if (script[2] === currentWorkspace) {
            script[2] = workspaces[1];
        }
    })

    // Delete
    workspaces = workspaces.filter(workspace => workspace !== currentWorkspace);
    
    workspaceLoad();
}

function workspaceChange(value) {
    workspaceList.value = value;

    // if "All" leave currentWorkspace as is but show all scripts
    currentWorkspace = (value === workspaces[0] ? currentWorkspace : value);

    const scriptTabs = document.getElementById("scripts-tab").getElementsByTagName("LI");

    for (element of scriptTabs) {
        const tabId = element.id;

        // Show only scripts in current workspace
        // if "All" show all scripts (but workspace that gets saved is the one previously
        if (value === workspaces[0] || currentWorkspace === scripts[tabId][2]) {
            element.classList.remove("hide-script");
        } else {
            element.classList.add("hide-script");

            if (element.getElementsByClassName("active").length > 0) {
                scriptSelect(false);
            }
        }
        
    }
}

// Export all scripts in workspace
function workspaceExport() {
    var exported = [];
    var err = false;
    output.copy = undefined;

    scripts.forEach(script => {
        if (!err && (script[2] == currentWorkspace || workspaceList.value == workspaces[0])) {
            var result = runLua("workspace", script[0], script[1]);
            if (!result.status) {
                err = true;
                return;
            }
            exported.push(result.output);
        }
    });

    if (err) {
        return;
    }
    var text = exported.join(";");

    if (text.length == 0) {
        output.value = "There are no scripts here";
        return;
    }

    output.value = text;
    output.copy = 0;
}

function workspaceMoveScript() {
    if (!activeTab) return;

    scripts[activeTab.id][2] = currentWorkspace;
    scriptSave();
}