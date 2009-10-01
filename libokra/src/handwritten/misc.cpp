// misc.cpp
//
// author: Erik Winkels (aerique@xs4all.nl)
//
// See the LICENSE file in the Okra root directory for more info.

#include "okra.h"


// Prototypes

extern "C"
{
    bool hw_conan (array_shuttle*);
    Ray* hw_create_ray (okraReal, okraReal, okraReal,
                        okraReal, okraReal, okraReal);
    void hw_free (void*);  // sigh...
    array_shuttle* hw_get_available_renderers (Root*);
    OverlayManager* hw_get_overlay_manager_singleton_ptr ();
    ResourceGroupManager* hw_get_resource_group_manager_singleton_ptr ();
    unsigned long hw_get_window_handler (RenderWindow*);
    void* hw_malloc (size_t);  // sigh...
    ManualObject* hw_manual_object (SceneManager*, const char*, const char*,
                                    okraReal[], int);
    void hw_manual_object_triangle (ManualObject*, okraReal[]);
    MaterialPtr hw_material_clone (Material*, const char*);
    void hw_material_set_ambient (Material*, okraReal, okraReal, okraReal);
    void hw_material_set_diffuse (Material*, okraReal, okraReal, okraReal,
                                  okraReal);
    void hw_material_set_self_illumination (Material*, okraReal, okraReal,
                                            okraReal);
    Ray* hw_ray_constructor ();
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


Ray* hw_create_ray (okraReal x_origin, okraReal y_origin, okraReal z_origin,
                    okraReal x_dir, okraReal y_dir, okraReal z_dir)
{
  return new Ray(Vector3(x_origin, y_origin, z_origin),
                 Vector3(x_dir, y_dir, z_dir));
}


// XXX: idiot, you don't have to define a wrapper for this...
void hw_free (void* memory)
{
    free(memory);
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


// XXX: idiot, you don't have to define a wrapper for this...
void* hw_malloc (size_t size)
{
    return malloc(size);
}


ManualObject* hw_manual_object (SceneManager* sm, const char* name,
                                const char* material, okraReal floats[],
                                int elements)
{
    ManualObject* mo = sm->createManualObject(name);
    mo->begin(material, RenderOperation::OT_TRIANGLE_LIST);

    for (int i = 0; i <= (elements * 3 * 2); i += 18)
    {
        mo->position(floats[i+0], floats[i+1], floats[i+2]);
        mo->normal(floats[i+3], floats[i+4], floats[i+5]);
        mo->position(floats[i+6], floats[i+7], floats[i+8]);
        mo->normal(floats[i+9], floats[i+10], floats[i+11]);
        mo->position(floats[i+12], floats[i+13], floats[i+14]);
        mo->normal(floats[i+15], floats[i+16], floats[i+17]);
    }

    mo->end();
    return mo;
}


void hw_manual_object_triangle (ManualObject* mo, okraReal floats[])
{
    mo->position(floats[0], floats[1], floats[2]);
    mo->normal(floats[3], floats[4], floats[5]);
    mo->position(floats[6], floats[7], floats[8]);
    mo->normal(floats[9], floats[10], floats[11]);
    mo->position(floats[12], floats[13], floats[14]);
    mo->normal(floats[15], floats[16], floats[17]);
    //mo->position(floats[0], floats[1], floats[2]);
    //mo->normal(floats[3], floats[4], floats[5]);
    //mo->textureCoord(floats[6], floats[7]);
    //mo->position(floats[8], floats[9], floats[10]);
    //mo->normal(floats[11], floats[12], floats[13]);
    //mo->textureCoord(floats[14], floats[15]);
    //mo->position(floats[16], floats[17], floats[18]);
    //mo->normal(floats[19], floats[20], floats[21]);
    //mo->textureCoord(floats[22], floats[23]);
}

MaterialPtr hw_material_clone (Material* m, const char* new_name)
{
  return m->clone(new_name);
}


void hw_material_set_ambient (Material* m, okraReal r, okraReal g, okraReal b)
{
    m->setAmbient(r, g, b);
}


void hw_material_set_diffuse (Material* m, okraReal r, okraReal g, okraReal b,
                              okraReal a)
{
    m->setDiffuse(r, g, b, a);
}


void hw_material_set_self_illumination (Material* m, okraReal r, okraReal g,
                                        okraReal b)
{
    m->setSelfIllumination(r, g, b);
}


Ray* hw_ray_constructor ()
{
  return new Ogre::Ray();
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
