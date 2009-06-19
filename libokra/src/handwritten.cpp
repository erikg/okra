// handwritten.cpp
//
// author: Erik Winkels (aerique@xs4all.nl)
//
// See the LICENSE file in the Okra root directory for more info.

#include "Ogre.h"
#include "okra.h"

using namespace Ogre;


// Prototypes

extern "C"
{
    bool hw_conan (array_shuttle*);
    array_shuttle* hw_get_available_renderers (Root*);
    OverlayManager* hw_get_overlay_manager_singleton_ptr ();
    ResourceGroupManager* hw_get_resource_group_manager_singleton_ptr ();
    unsigned long hw_get_window_handler(RenderWindow*);
    Root* hw_root_constructor (const char*, const char*, const char*);
    void hw_message_pump ();
    Viewport* hw_simple_add_viewport (RenderWindow*, Camera*);
}


// Functions

bool hw_conan (array_shuttle* as)
{
    std::cout << "[hw_conan] Called for type: " << as->type << " with length "
              << as->length << "..." << std::endl;
    switch (as->type)
    {
        case OKRA_INT:
            std::cout << "[hw_conan] in: case OKRA_INT..." << std::endl;
            delete [] (as->int_array);
            break;
        case OKRA_POINTER:
            std::cout << "[hw_conan] in: case OKRA_POINTER..." << std::endl;
            delete [] (as->pointer_array);
            break;
        case OKRA_REAL:
            std::cout << "[hw_conan] in: case OKRA_REAL..." << std::endl;
            delete [] (as->real_array);
            break;
        case OKRA_STRING:
            std::cout << "[hw_conan] in: case OKRA_STRING..." << std::endl;
            for (size_t i = 0; i < as->length; i++)
            {
                std::cout << "[hw_conan] deleting at index " << i << "..."
                          << std::endl;
                delete [] ((as->string_array)[i]);
            }
            break;
        default:
            std::cout << "[hw_conan] Unknown okra_type: " << as->type
                      << std::endl;
            return false;
    }
        std::cout << "[hw_conan] deleting array_shuttle..." << std::endl;
    delete as;
    return true;
}


array_shuttle* hw_get_available_renderers (Root* ogre_root)
{
    array_shuttle* as = new array_shuttle;
    RenderSystemList* rsl = ogre_root->getAvailableRenderers();

    as->length = rsl->size();
    as->type = OKRA_STRING;
    as->string_array = new const char*[rsl->size()];

    for (size_t i = 0; i < rsl->size(); i++)
    {
        RenderSystem* rs = rsl->at(i);
        String name = rs->getName();
        (as->string_array)[i] = new char[name.length()+1];
        (as->string_array)[i] = name.c_str();
    }

    return as;
}


OverlayManager* hw_get_overlay_manager_singleton_ptr ()
{
    return OverlayManager::getSingletonPtr();
}


ResourceGroupManager* hw_get_resource_group_manager_singleton_ptr ()
{
    return ResourceGroupManager::getSingletonPtr();
}


unsigned long hw_get_window_handler (RenderWindow* rw)
{
    unsigned long hWnd;
    rw->getCustomAttribute("WINDOW", &hWnd);
    return hWnd;
}


Root* hw_root_constructor (const char* plugin, const char* config,
                           const char* log)
{
    return new Root(plugin, config, log);
}


unsigned long hw_get_window_handler (RenderTarget* rt)
{
    unsigned long hWnd;
    rt->getCustomAttribute("WINDOW", &hWnd);
    return hWnd;
}


void hw_message_pump()
{
    WindowEventUtilities::messagePump();
}


//ResourceGroupManager* hw_resource_group_manager_get_singleton_ptr ()
//{
//    return ResourceGroupManager::getSingletonPtr();
//}


Viewport* hw_simple_add_viewport (RenderWindow* rw, Camera* cam)
{
    return rw->addViewport(cam);
}
