#include <kyfoo/SystemError.hpp>

#include "win32.hpp"

namespace kyfoo {

//
// SystemError

SystemError SystemError::last()
{
    return ::GetLastError();
}

//
// ErrorMessage

SystemErrorMessage::SystemErrorMessage(SystemError err)
{
    myCard = ::FormatMessageW(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                              0,
                              err.code(),
                              0,
                              (LPWSTR)&myData,
                              0,
                              NULL);
}

SystemErrorMessage::~SystemErrorMessage()
{
    ::LocalFree(myData);
}

} // namespace kyfoo
