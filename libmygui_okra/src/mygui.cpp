// mygui.cpp
//
// author: Erik Winkels (aerique@xs4all.nl)
//
// See the LICENSE file in the Okra root directory for more info.

#include "okra-mygui.h"


// Prototypes

extern "C"
{
    Gui* mygui_constructor ();

    void mygui_initialise (Gui*, Ogre::RenderWindow*);
    void mygui_load_layout (const char*);
}


// Functions

Gui* mygui_constructor ()
{
    return new Gui();
}


void mygui_initialise (Gui* gui, Ogre::RenderWindow* orw)
{
    gui->initialise(orw);
}


void mygui_load_layout (const char* name)
{
    LayoutManager::getInstance().load(name);
}
