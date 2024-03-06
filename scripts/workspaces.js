"use strict";
// This code treats 0 and 1 as reserved = All and Default
let workspaces = ["All", "Default Workspace"];
let currentWorkspace = workspaces[1];
let movingScript = false;

const workspaceList = document.getElementById("workspaceList");
const moveButton = document.getElementById("move-script");

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

    // If "All"
    if (currentWorkspace === workspaces[0]) {
        alert(`Cannot rename ${currentWorkspace}`);
        return
    }
    // If "Default"
    if (currentWorkspace === workspaces[1]) {
        output.value = "Hidden function: Running unittests...";
        output.copy = undefined;
        runLua("unittest");
        return;
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

    // Can't delete default
    if (currentWorkspace === workspaces[1]) {
        alert(`Cannot delete ${currentWorkspace}`);
        return;
    }

    const tabs = document.getElementById("scripts-tab");
    if (currentWorkspace === workspaces[0]) {
        if (!confirm(`This will delete ***ALL*** scripts and workspaces!\nTHIS CANNOT BE UNDONE!!`)) return;

        scripts = [];
        while (tabs.firstChild) {
            tabs.removeChild(tabs.lastChild);
        }
        workspaces.splice(2);
    } else {
        const count = scripts.filter(script => script[2] === currentWorkspace).length;
        const msg = `Are you sure you want to delete workspace ${currentWorkspace}` +
          (count > 0 ? ` and its ${count} child(ren)?` : "?");
        if (!confirm(msg)) return;

        // Delete all scripts from workspace
        // Have to fix all the element ids first.
        let target = 0;
        for (let i = 0; i < scripts.length; ++i) {
            if (scripts[i][2] !== currentWorkspace) {
                scripts[target] = scripts[i];
                tabs.children[target].id = target;
                target++;
            } else {
                tabs.removeChild(tabs.children[target]);
            }
        }
        scripts.length = target;

        // Delete the workspace itself
        workspaces = workspaces.filter(workspace => workspace !== currentWorkspace);
    }
    workspaceLoad();

    activeTab = false;
    scriptSelect(false);
}

function workspaceChange(value) {
    workspaceList.value = value;

    // if "All" leave currentWorkspace as is but show all scripts
    // Otherwise, do move-script processing
    if (value !== workspaces[0]) {
      currentWorkspace = value;
      if (movingScript) {
        scripts[movingScript.id][2] = value;
        scriptSave();
        workspaceCancelMove();
      }
    }

    const scriptTabs = document.getElementById("scripts-tab").getElementsByTagName("LI");

    for (const element of scriptTabs) {
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
    output.value = " ";
    output.copy = undefined;
    let script_list = [];

    for (const script of scripts) {
        if (script[2] === currentWorkspace || workspaceList.value === workspaces[0]) {
            script_list.push({name: script[0], text: script[1]})
        }
    }
    runLua("workspace", script_list);
}

function workspaceExportDone(result) {
    if (!result.status) {
        return;
    }

    if (result.value.length == 0) {
        output.value = "There are no scripts here, or they are all libraries (produce no code)";
        return;
    }

    output.value = result.value;
    output.copy = 0;
}

function workspaceMoveScript() {
    if (!activeTab) return;
    if (!movingScript) {
      movingScript = activeTab;
      moveButton.classList.add("basicBtnCyan");
    } else {
      workspaceCancelMove();
    }
}

function workspaceCancelMove() {
    moveButton.classList.remove("basicBtnCyan");
    movingScript = false;
}

function workspaceImportSource(jsonObj) {
    if (!Object.prototype.hasOwnProperty.call(jsonObj, "workspaces")) {
        output.value = "Bad JSON passed to import";
        output.copy = undefined;
    }
    let workspaceName = "All";  // Expand scope so we can see the last element.
    for (workspaceName in jsonObj.workspaces) {
        const wsData = jsonObj.workspaces[workspaceName];
        for (const script of wsData) {
            scriptNew(script[0]);
            scripts[scripts.length - 1][1] = script[1]
            scripts[scripts.length - 1][2] = workspaceName;
        }
    }
    scriptSave();
    workspaceLoad();
    workspaceChange(workspaceName);
}

// Export all source in workspace
function workspaceExportSource() {
    let outputWorkspaces = {};
    if (workspaceList.value == workspaces[0]) {
        for (let i = 1; i < workspaces.length; ++i) {
            outputWorkspaces[workspaces[i]] = [];
        };
        scripts.forEach(script => {
            outputWorkspaces[script[2]].push([script[0], script[1]]);
        });
    } else {
        outputWorkspaces[currentWorkspace] = [];
        scripts.forEach(script => {
            if (script[2] == currentWorkspace) {
                outputWorkspaces[script[2]].push([script[0], script[1]]);
            }
        });
    }
    output.value = JSON.stringify({workspaces: outputWorkspaces});
    output.copy = 0;
}

// Instructions on how to export from another website
function workspaceOtherSite() {
    output.value = `
If you want to export your scripts from another website that doesn't have an "Export Source" button, you can paste the following code into the console on that site. This will put all your code into the output textbox, in a form that you can import here.
(Pasting random code into the console is a generally dangerous thing to do, so read over the code and at least have vague confidence that it isn't doing anything evil, first!)

let outw = {};
for (let ws of workspaces.slice(1)) outw[ws] = [];
for (let script of scripts) outw[script[2]].push(script.slice(0, 2));
output.value = JSON.stringify({workspaces: outw});
output.copy = 0;
    `.trim();
    output.copy = output.value.match(/let outw/).index;
}
